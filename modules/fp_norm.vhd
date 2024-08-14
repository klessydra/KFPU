
-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;

entity fp_norm is  
  generic (
    size          : natural;
    exponent_size : natural;
    mantissa_size : natural;
    bias          : natural
  );
  port( 
    clk_i         : in  std_logic;
    rst_ni        : in  std_logic;
    op_mode       : in  std_logic_vector(FP_INSTR_LEN-1 downto 0);
    round_mode    : in  std_logic_vector(7 downto 0);
    norm_a        : in  std_logic;
    norm_b        : in  std_logic;
    inf_a         : in  std_logic;
    inf_b         : in  std_logic;
    nan_a         : in  std_logic;
    nan_b         : in  std_logic;
    inexact_norm  : in  std_logic;
    comp_exp      : in  std_logic_vector(1 downto 0); 
    comp_a_b      : in  std_logic_vector(1 downto 0);
    exp_int       : in  std_logic_vector(exponent_size+1 downto 0);
    exp_shift     : in  std_logic_vector(exponent_size downto 0);
    mnt_res_add   : in  std_logic_vector(mantissa_size+3 downto 0);
    mnt_res_mul   : in  std_logic_vector((mantissa_size+1)*2+1 downto 0);
    sign_res      : in  std_logic;
    bypass        : in  std_logic;
    result_bypass : in  std_logic_vector(size-1 downto 0);
    underflow     : in  std_logic;
    overflow_norm : out std_logic;
    precision     : out std_logic;
    inexact       : out std_logic;
    result_ru     : out std_logic_vector(size-1 downto 0)
  );
end entity;

architecture behavioral of fp_norm is

  signal mantissa_norm_int   : std_logic_vector((mantissa_size+1)*2+1 downto 0);
  signal mantissa_norm       : std_logic_vector(1+2*mantissa_size + (mantissa_size+1)*2+1 downto 0);

  signal exp_sum_int         : std_logic_vector(exponent_size+1 downto 0);
  signal exp_sum             : std_logic_vector(exponent_size downto 0);

  signal mantissa            : std_logic_vector(mantissa_size-1 downto 0);

  signal overflow_en         : std_logic;
  
  signal implicit_bit        : std_logic;

begin

  overflow_en <= '0' when  round_mode(RTZ_bit_position) = '1' or
                          (round_mode(RUP_bit_position) = '1' and sign_res = '1') or 
                          (round_mode(RDN_bit_position) = '1' and sign_res = '0') else
                 '1';

  MNT_HNDL : process(all)
  begin

    --mantissa_norm_int <= mnt_res_mul;
    if (op_mode(0) or op_mode(1)) then
      mantissa_norm_int <= (mantissa_size+4 to (mantissa_size+1)*2+1 => '0') & mnt_res_add;
    elsif op_mode(2) then
      mantissa_norm_int <= mnt_res_mul;
    end if;

    mantissa_norm <= mantissa_norm_int & (0 to 2*mantissa_size => '0');

    if norm_a or norm_b or op_mode(2) then -- normalize the mantissa and store it in mantissa_norm_int
      -- Step 1: normalize the input
      if (op_mode(0) or op_mode(1)) then -- add or sub 
        for i in mantissa_size+3 downto 0 loop
          if mnt_res_add(i) = '1' then
            if i = mantissa_size+3 then
              mantissa_norm_int(mantissa_size+3 downto 0) <= mnt_res_add srl 1;
              exit;
            elsif i = mantissa_size+2 then
              exit;
            else
              mantissa_norm_int(mantissa_size+3 downto 0) <= mnt_res_add sll (mantissa_size+3-i-1);
              exit;
            end if;
          elsif i = 0 then
          end if;
        end loop;
      elsif op_mode(2) then
        for i in (mantissa_size*2)+1 downto 0 loop
          if mnt_res_mul(i) = '1' then
            if i = (mantissa_size*2)+1 then
              mantissa_norm_int <= mnt_res_mul srl 1;
              exit;
            elsif i = (mantissa_size*2) then
              exit;
            else
              mantissa_norm_int <= mnt_res_mul sll ((mantissa_size*2)-i);
              exit;
            end if;
          elsif i = 0 then
          end if;
        end loop;
      end if;

      -- Step 2: Correct the normalized input if the exponent overflows or underflows
      if overflow_norm = '1' then
        if overflow_en then
          mantissa_norm(mantissa_size+2 + 1+2*mantissa_size downto 2 + 1+2*mantissa_size) <= (others => '0'); -- zero out the bits that will map to the mantissa signal
        else
          mantissa_norm(mantissa_size+2 + 1+2*mantissa_size downto 2 + 1+2*mantissa_size) <= (others => '1'); -- max out the bits that will map to the mantissa signal
        end if;
      else
        -- AAA very inefficient design as for DP FP, the BIAS is 1023 
        for i in 0 to (bias-1)+(mantissa_size*2) loop
          if signed(exp_sum_int) = -i then
            mantissa_norm <= (mantissa_norm_int & (0 to 2*mantissa_size => '0')) srl i+1;
          end if;
        end loop;
      end if;
    end if;
  end process;

  process(all)
  begin
    -- Step 1: Determine how much the exponent will be after normalizing 
    exp_sum_int <= std_logic_vector(signed(exp_int) + signed(exp_shift(exponent_size) & exp_shift)); -- sign extend exp_shift by 1-bit

    -- Step 2: Correct the exponent in case of overflows or underflows
    overflow_norm <= '0';
    exp_sum <= exp_sum_int(exponent_size downto 0);
    if exp_sum_int(exponent_size+1) = '1' then  -- MSB indicates that the exponent is negative
      exp_sum <= (others => '0');
    elsif signed(exp_sum_int) = 1 then -- if the exponent is equal to 1, we set the LSB to zero if the implicit bit is '0', else '1' if otherwise 
      exp_sum <= (0 to exponent_size-1 => '0') & implicit_bit;
    elsif exp_sum_int(exponent_size downto 0) >= '0' & (exponent_size-1 downto 0 => '1') then -- else if the sum is positive and greater then 1, we set the exponent and check if for overflow
      if overflow_en then
        exp_sum(exponent_size-1 downto 0) <= (exponent_size-1 downto 0 => '1');
      else
        exp_sum(exponent_size-1 downto 0) <= (exponent_size-1 downto 1 => '1') & '0';
      end if;
      overflow_norm <= '1';
    end if;
  end process;

  implicit_bit <= mantissa_norm_int(mantissa_size+2) when or_vect_bits(op_mode(1 downto 0)) else
                  mantissa_norm_int(mantissa_size*2);

  inexact <= (mantissa_norm(0  + 1+2*mantissa_size) or (mnt_res_add(0) and mnt_res_add(mantissa_size+3))) or inexact_norm  when (op_mode(0) or op_mode(1)) else
             (or_vect_bits(mantissa_norm(1 + 1+2*mantissa_size downto 0)) or  (mnt_res_mul(0) and mnt_res_mul(mantissa_size*2+1)) or inexact_norm);

  mantissa     <= mantissa_norm(mantissa_size+1 + 1+2*mantissa_size downto 2 + 1+2*mantissa_size) when (op_mode(0) or op_mode(1)) else
                  mantissa_norm(mantissa_size+2 + 1+2*mantissa_size downto 3 + 1+2*mantissa_size) when  op_mode(2);

  precision    <= mantissa_norm(1 + 1+2*mantissa_size) when (op_mode(0) or op_mode(1)) else
                  mantissa_norm(2 + 1+2*mantissa_size) when op_mode(2);


  --INEXACT_CHECK : process(all)
  --begin
  --  inexact <= inexact_int2; -- check the LSB after normalizing which determines if inexact, also if the sum of the mantissas has the LSB 1 we check if the MSB is 1 to know if the inexact bit will be shifted out, else we check the input if some bits were shifted out after aligning the mantissas
  --end process;

  result_ru <= sign_res & exp_sum(exponent_size-1 downto 0) & mantissa when bypass = '0' else result_bypass;

end architecture; -- arch