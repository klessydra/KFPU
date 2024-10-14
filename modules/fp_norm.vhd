
-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

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
    clk_i           : in  std_logic;
    rst_ni          : in  std_logic;
    op_mode         : in  std_logic_vector(FP_INSTR_LEN-1 downto 0);
    round_mode      : in  std_logic_vector(7 downto 0);
    norm_a          : in  std_logic;
    norm_b          : in  std_logic;
    inf_a           : in  std_logic;
    inf_b           : in  std_logic;
    nan_a           : in  std_logic;
    nan_b           : in  std_logic;
    inexact_norm    : in  std_logic;
    comp_exp        : in  std_logic_vector(1 downto 0); 
    comp_a_b        : in  std_logic_vector(1 downto 0);
    exp_int         : in  std_logic_vector(exponent_size+1 downto 0);
    exp_shift       : in  std_logic_vector(exponent_size downto 0);
    mnt_res_add     : in  std_logic_vector(mantissa_size+3 downto 0);
    mnt_res_mul     : in  std_logic_vector((mantissa_size+1)*2+1 downto 0);
    sign_res        : in  std_logic;
    bypass          : in  std_logic;
    result_bypass   : in  std_logic_vector(size-1 downto 0);
    underflow       : in  std_logic;
    uf_3_trig       : out std_logic; -- trigger for the third condition of underflow (very special case)
    overflow_norm   : out std_logic;
    precision_round : out std_logic;
    inexact_round   : out std_logic;
    result_ru       : out std_logic_vector(size-1 downto 0)
  );
end entity;

architecture behavioral of fp_norm is

  constant size_width        : integer := integer(ceil(log2(real((mantissa_size*2)+2))));
  signal   clz               : std_logic_vector(size_width downto 0);
  signal   clz2              : std_logic_vector(size_width downto 0);

  signal mantissa_norm_int   : std_logic_vector((mantissa_size+1)*2+1 downto 0);
  signal mantissa_norm       : std_logic_vector(1+mantissa_size + (mantissa_size+1)*2+1 downto 0);

  signal inexact_int         : std_logic;

  signal exp_sum_int         : std_logic_vector(exponent_size+1 downto 0);
  signal exp_sum             : std_logic_vector(exponent_size downto 0);

  signal mantissa            : std_logic_vector(mantissa_size-1 downto 0);

  signal overflow_en         : std_logic;
  
  signal implicit_bit        : std_logic;

  component CLZ_top is
    generic (
      size       : natural;
      size_width : integer
    );
    Port (
      input  : in  std_logic_vector(size-1 downto 0);
      output : out std_logic_vector(size_width downto 0)
    );
  end component;

  -- Function to reverse a std_logic_vector
  function reverse_vector(input_vector : std_logic_vector) return std_logic_vector is
    variable reversed_vector : std_logic_vector(input_vector'range);
  begin
    for i in input_vector'range loop
      reversed_vector(i) := input_vector(input_vector'left + input_vector'right - i);
    end loop;
    return reversed_vector;
  end function;

begin

  overflow_en <= '0' when  round_mode(RTZ_bit_position) = '1' or
                          (round_mode(RUP_bit_position) = '1' and sign_res = '1') or 
                          (round_mode(RDN_bit_position) = '1' and sign_res = '0') else
                 '1';


  --LEAD_Z_Inst : CLZ_top
  --  generic map(
  --    size       => (mantissa_size*2)+2,
  --    size_width => size_width
  --  )
  --  port map (
  --    input  => mnt_res_mul((mantissa_size*2)+1 downto 0),
  --    output => clz
  --  );

  -- Use the reverse_vector function in the LAG_Z_Inst
  --LAG_Z_Inst : CLZ_top
  --  generic map(
  --    size       => (mantissa_size*2)+2,
  --    size_width => size_width
  --  )
  --  port map (
  --    input  => reverse_vector(mnt_res_mul((mantissa_size*2)+1 downto 0)),
  --    output => clz2
  --  );

  MNT_HNDL : process(all)
  begin

    inexact_int <= '0';

    --mantissa_norm_int <= mnt_res_mul;
    if (op_mode(0) or op_mode(1)) then
      mantissa_norm_int <= (mantissa_size+4 to (mantissa_size+1)*2+1 => '0') & mnt_res_add;
    elsif op_mode(2) then
      mantissa_norm_int <= mnt_res_mul;
    end if;

    mantissa_norm <= mantissa_norm_int & (0 to mantissa_size => '0');

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
      elsif op_mode(2) then -- mul
        if mnt_res_mul((mantissa_size*2)+1) = '1' then
          mantissa_norm_int <= mnt_res_mul srl 1;
        elsif unsigned(std_logic_vector(-signed(exp_shift))) > 0 then -- shift left for CLZ>1. Note: in case CLZ=1, it takes mnt_res_mul without shift
          mantissa_norm_int <= mnt_res_mul sll to_integer(unsigned(std_logic_vector(-signed(exp_shift))));
        end if;
      end if;

      -- Step 2: Correct the normalized input if the exponent overflows or underflows
      if overflow_norm = '1' then
        if overflow_en then
          mantissa_norm(mantissa_size+2 + 1+mantissa_size downto 2 + 1+mantissa_size) <= (others => '0'); -- zero out the bits that will map to the mantissa signal
        else
          mantissa_norm(mantissa_size+2 + 1+mantissa_size downto 2 + 1+mantissa_size) <= (others => '1'); -- max out the bits that will map to the mantissa signal
        end if;
      else
        if (signed(exp_sum_int) < -mantissa_size) then
          inexact_int <= or_vect_bits(mantissa_norm_int(7 downto 0));
          mantissa_norm <= (others => '0');
        else
          -- AAA maybe replace this loop with a CLZ?
          for i in 0 to mantissa_size loop
            if signed(exp_sum_int) = -i then
              mantissa_norm <= (mantissa_norm_int & (0 to mantissa_size => '0')) srl i+1;
            end if;
          end loop;
        end if;
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

  -- This signal triggers the UF flag for the condition when the result is inexact but after rounding the result becomes the smallest normalized number.
  -- For RNE and RMM:
     -- The number is allowed to be above the middle but only "mantissa_size-1" inexact bits after the precision are considered, and not all the bits
     -- it is triggered when the precision is '1' and the "mantissa_size-1" bits are '0', other subsequent inexact bits after those two are ignored meaning they can be even'1' (ming the result bigger then the middle part) 
  -- For RDN and RUP:
    -- If both precision_round and inexact_round are set, then it is not triggered 
  -- For RTZ:
    -- The trigger of this flag is ignored because we will never round up towards the smallest normalized number
  uf_3_trig <= precision_round and not or_vect_bits(mantissa_norm(2*mantissa_size-1 downto 2*mantissa_size-mantissa_size+1)) when round_mode(RNE_bit_position) or round_mode(RMM_bit_position) else -- RDN or RUP (RTZ also goes there, but it has no effect on the outcome whether it goes or not)
          not (precision_round and     inexact_round);

  implicit_bit <= mantissa_norm_int(mantissa_size+2) when or_vect_bits(op_mode(1 downto 0)) else
                  mantissa_norm_int(mantissa_size*2);

  inexact_round <=                (mantissa_norm(0 + 1+mantissa_size)         or (mnt_res_add(0) and mnt_res_add(mantissa_size+3)))  or inexact_norm  when (op_mode(0) or op_mode(1)) else
      (inexact_int or or_vect_bits(mantissa_norm(2*mantissa_size-1 downto 0)) or (mnt_res_mul(0) and mnt_res_mul(mantissa_size*2+1)) or inexact_norm);

  mantissa <= mantissa_norm(mantissa_size+1 + 1+mantissa_size downto 2 + 1+mantissa_size) when (op_mode(0) or op_mode(1)) else
              mantissa_norm(2*mantissa_size+mantissa_size downto 2*mantissa_size+1); -- when op_mode(2)

  precision_round <= mantissa_norm(1 + 1+mantissa_size) when (op_mode(0) or op_mode(1)) else
                     mantissa_norm(2*mantissa_size); -- when op_mode(2)

  --INEXACT_CHECK : process(all)
  --begin
  --  inexact <= inexact_int2; -- check the LSB after normalizing which determines if inexact, also if the sum of the mantissas has the LSB 1 we check if the MSB is 1 to know if the inexact bit will be shifted out, else we check the input if some bits were shifted out after aligning the mantissas
  --end process;

  result_ru <= sign_res & exp_sum(exponent_size-1 downto 0) & mantissa when bypass = '0' else result_bypass;

end architecture; -- arch