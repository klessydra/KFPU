
-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;

entity fp_mul is  
  generic (
    size          : natural := 8;
    exponent_size : natural := 4;
    mantissa_size : natural := 3;
    bias          : integer := 7
  );
  port (
    clk_i             : in  std_logic;
    rst_ni            : in  std_logic;
    mul_en            : in  std_logic;
    mode              : in  std_logic;
    data_a            : in  std_logic_vector(size-1 downto 0);
    data_b            : in  std_logic_vector(size-1 downto 0);
    sign_a            : in  std_logic;
    sign_b            : in  std_logic;
    exp_a             : in  std_logic_vector(exponent_size-1 downto 0);
    exp_b             : in  std_logic_vector(exponent_size-1 downto 0);
    mnt_a             : in  std_logic_vector(mantissa_size-1 downto 0);
    mnt_b             : in  std_logic_vector(mantissa_size-1 downto 0);
    zero_a            : in  std_logic;
    zero_b            : in  std_logic;
    norm_a            : in  std_logic;
    norm_b            : in  std_logic;
    inf_a             : in  std_logic;
    inf_b             : in  std_logic;
    nan_a             : in  std_logic;
    nan_b             : in  std_logic;
    snan_a            : in  std_logic;
    snan_b            : in  std_logic;
    opp_signs         : in  std_logic; -- indicates whether the signs are equal or opposite
    comp_a_b          : in  std_logic_vector(1 downto 0);
    precision         : out std_logic;
    inexact_mul       : out std_logic;
    invalid_mul       : out std_logic;
    exp_mul_int       : out std_logic_vector(exponent_size+1 downto 0);
    exp_shift_mul     : out std_logic_vector(exponent_size downto 0);
    mnt_res_mul       : out std_logic_vector((mantissa_size+1)*2+1 downto 0);
    sign_res_mul      : out std_logic; -- contains the sign bit of the outputs
    bypass_mul        : out std_logic;
    result_bypass_mul : out std_logic_vector(size-1 downto 0);
    underflow_mul     : out std_logic;
    overflow_mul      : out std_logic;
    ready_mul         : out std_logic
  );
end entity fp_mul;

architecture floating_point_multiplier of fp_mul is

  --constant exp_shift_ceil    : natural := integer(ceil(log2(real(mantissa_size))));

  --signal exp_sum_int         : std_logic_vector(exponent_size+1 downto 0);
  signal exp_sum_int2        : std_logic_vector(exponent_size+1 downto 0);
  signal exp_sum_post_int    : std_logic_vector(exponent_size+1 downto 0);
  signal exp_sum_post        : std_logic_vector(exponent_size+1 downto 0);

  signal bias_compensate     : std_logic;
  signal ext_mantissa_a      : std_logic_vector(mantissa_size+1 downto 0);
  signal ext_mantissa_b      : std_logic_vector(mantissa_size+1 downto 0);
  signal full_mantisssa_a    : std_logic_vector(mantissa_size+1 downto 0);
  signal full_mantisssa_b    : std_logic_vector(mantissa_size+1 downto 0);
  signal mantissa_norm_int   : std_logic_vector((mantissa_size+1)*2+1 downto 0);
  signal mantissa_norm       : std_logic_vector((mantissa_size+1)*2+1 downto 0);
  signal mantissa_res        : std_logic_vector(mantissa_size-1 downto 0);

begin

  ready_mul <= mul_en;

  sign_res_mul <= opp_signs;

  process(all)
  begin
    -- Step 1: Extend the mantissa with the implicit bit and a sign bit
    ext_mantissa_a <= '0' & norm_a & mnt_a;
    ext_mantissa_b <= '0' & norm_b & mnt_b;
    -- Step 2 : Multiply the extended mantissas
    mnt_res_mul <= std_logic_vector(signed(ext_mantissa_a) * signed(ext_mantissa_b));
  end process;

  process(all)
  begin
    precision       <= '0';
    inexact_mul     <= '0';
    underflow_mul   <= '0';
    overflow_mul    <= '0';
    bias_compensate <= '0';
    if norm_a nand norm_b then
      bias_compensate <= '1'; -- bias compensation needed when to make the exponent in the denormalized number equal to the normalized
    end if;
    -- Step 1: Extend the exponents with two bits, the MSB being the sign bit, and the MSB-1 is the overflow bit, and add them together, removing the bias from one of them as to not let the bias in both exponents be added twice. Also add a bias compensate bit that becomes '1' when one input is notmalized and the second is denormalized.
    exp_mul_int <= std_logic_vector(unsigned("00" & exp_a) + unsigned("00" & exp_b) + bias_compensate - bias);
    -- Step 2: If the sum of the compensated exponents above is less then "1-mantissa_size" then we are sure that the result is going to be an underflow, else we have to make extra checks
    if signed(exp_mul_int) <= 0 then
      inexact_mul <= or_vect_bits(mnt_res_mul(mantissa_size-1 downto 0));
    end if;
    if (exp_mul_int(exponent_size+1) = '1') then
      --exp_mul_int <= (others => '0'); -- denormalized numbers should all enter in here
      precision   <= '0'; -- AAA check if preciion is 0 here
      if signed(exp_mul_int) <= -4 then
        underflow_mul   <= '1';
      end if;
    elsif exp_mul_int(exponent_size) = '1' then
      overflow_mul    <= '1';
      --exp_mul_int(exponent_size-1 downto 1) <= (others => '1') & '0';
    end if;
  end process;

  process(all)
  begin
    for i in (mantissa_size*2)+1 downto 0 loop -- Start with the MSB that is not a sign bit
      if mnt_res_mul(i) = '1' then
        if i = (mantissa_size*2)+1 then
          exp_shift_mul     <= (1 to exponent_size => '0') & '1';
          exit;
        elsif i = (mantissa_size*2) then
          exp_shift_mul     <= (others => '0');
          exit;
        else
          exp_shift_mul <= std_logic_vector(to_signed(-((mantissa_size*2)-i),exponent_size+1));
          exit;
        end if;
      elsif i = 0 then
        exp_shift_mul <= "11" & (0 to exponent_size-2 => '0');
      end if;
    end loop;
  end process;

  process(all)
  begin
    bypass_mul <= '0';
    invalid_mul   <= '0'; 
    if nan_a or nan_b then -- set the output to the canonical quiet nan (qnan)
      bypass_mul <='1';
      result_bypass_mul <= '0' & (0 to exponent_size-1 => '1') & '1' & (mantissa_size-2 downto 0 => '0'); -- The standard does not specify the sign of the NaN, hence negative NaN was chosen as a default QNaN to match the results of the Gold Model compiled by GCC
      if (snan_a or snan_b) then
        invalid_mul   <= mul_en; 
      end if;
    elsif inf_a xor inf_b then -- if one input is an infinity and the other is not
      bypass_mul <='1';
      if zero_a xor zero_b then
        result_bypass_mul <= '0' & (0 to exponent_size-1 => '1') & '1' & (mantissa_size-2 downto 0 => '0');
        invalid_mul   <= mul_en;
      elsif opp_signs then -- if a > b, then a is the infinity
        result_bypass_mul <= '1' & (0 to exponent_size-1 => '1') & (0 to mantissa_size-1 => '0'); -- negative infinity
      else
        result_bypass_mul <= '0' & (0 to exponent_size-1 => '1') & (0 to mantissa_size-1 => '0'); -- positive infinity
      end if;
    elsif inf_a = '1' and inf_b = '1'  then -- if both inputs are infinities
      bypass_mul <='1';
      if opp_signs = '0' then -- if we have a negative infinity and a positve infinity
        result_bypass_mul <= '0' & data_a(size-2 downto 0); -- else we route one of the infinities to the output (in this case we route a)
      else
        result_bypass_mul <= '1' & data_a(size-2 downto 0); -- opp_signs for the sign bit means we take the negative sign
      end if;
    --elsif overflow_mul = '1' then
    --  bypass_mul <='1';
    --  result_bypass_mul <= opp_signs & (0 to exponent_size-1 => '1') & (mantissa_size-1 downto 0 => '0');
    elsif underflow_mul = '1' then
      bypass_mul <='1';
      result_bypass_mul <= opp_signs & (0 to size-2 => '0');
    end if;
  end process;

  --process(clk_i, rst_ni)
  --begin
  --  if rst_ni = '0' then
  --  elsif rising_edge(clk_i) then
  --    
  --  end if;
  --end process;

end floating_point_multiplier;