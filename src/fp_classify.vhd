
-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;

entity fp_classify is
  generic (
    size          : natural;
    exponent_size : natural;
    mantissa_size : natural
  );
  port (
    clk_i            : in  std_logic;
    rst_ni           : in  std_logic;
    data_a           : in  std_logic_vector(size-1 downto 0);
    data_b           : in  std_logic_vector(size-1 downto 0);
    data_c           : in  std_logic_vector(size-1 downto 0);
    sign_a           : out std_logic;
    sign_b           : out std_logic;
    sign_c           : out std_logic;
    exp_a            : out std_logic_vector(exponent_size-1 downto 0);
    exp_b            : out std_logic_vector(exponent_size-1 downto 0);
    exp_c            : out std_logic_vector(exponent_size-1 downto 0);
    mnt_a            : out std_logic_vector(mantissa_size-1 downto 0);
    mnt_b            : out std_logic_vector(mantissa_size-1 downto 0);
    mnt_c            : out std_logic_vector(mantissa_size-1 downto 0);
    zero_a           : out std_logic;
    zero_b           : out std_logic;
    zero_c           : out std_logic;
    neg_zero_a       : out std_logic;
    neg_zero_b       : out std_logic;
    neg_zero_c       : out std_logic;
    norm_a           : out std_logic;
    norm_b           : out std_logic;
    norm_c           : out std_logic;
    max_exp_a        : out std_logic;
    max_exp_b        : out std_logic;
    max_exp_c        : out std_logic;
    inf_a            : out std_logic;
    inf_b            : out std_logic;
    inf_c            : out std_logic;
    nan_a            : out std_logic;
    nan_b            : out std_logic;
    nan_c            : out std_logic;
    qnan_a           : out std_logic;
    qnan_b           : out std_logic;
    qnan_c           : out std_logic;
    snan_a           : out std_logic;
    snan_b           : out std_logic;
    snan_c           : out std_logic;
    opp_signs        : out std_logic; -- indicates whether the signs are equal or opposite
    comp_exp         : out std_logic_vector(1 downto 0); 
    comp_a_b         : out std_logic_vector(1 downto 0) -- contains three sates that determine if u_a > u_b "00", u_a = u_b "01", u_a < u_b "10"
  );
end entity fp_classify;

architecture behavioral of fp_classify is

  signal exp_a_stat : std_logic_vector(1 downto 0); -- classifies the exponent
  signal exp_b_stat : std_logic_vector(1 downto 0); -- classifies the exponent
  signal exp_c_stat : std_logic_vector(1 downto 0); -- classifies the exponent

  signal mnt_a_stat : std_logic_vector(1 downto 0); -- classifies the mantissa
  signal mnt_b_stat : std_logic_vector(1 downto 0); -- classifies the mantissa
  signal mnt_c_stat : std_logic_vector(1 downto 0); -- classifies the mantissa

  signal max_exp_a_int  : std_logic; -- set when the exponent bits are all '1'
  signal max_exp_b_int  : std_logic; -- set when the exponent bits are all '1'
  signal max_exp_c_int  : std_logic; -- set when the exponent bits are all '1'

  signal unsigned_a : std_logic_vector(size-2 downto 0); -- used in comparing the magnitude of the input
  signal unsigned_b : std_logic_vector(size-2 downto 0); -- used in comparing the magnitude of the input
  signal unsigned_c : std_logic_vector(size-2 downto 0); -- used in comparing the magnitude of the input

begin

  sign_a <= data_a(size-1);
  sign_b <= data_b(size-1);
  sign_c <= data_c(size-1);

  exp_a <= data_a(size-2 downto size-2-exponent_size+1);
  exp_b <= data_b(size-2 downto size-2-exponent_size+1);
  exp_c <= data_b(size-2 downto size-2-exponent_size+1);

  mnt_a <= data_a(size-2-exponent_size downto 0);
  mnt_b <= data_b(size-2-exponent_size downto 0);
  mnt_c <= data_c(size-2-exponent_size downto 0);

  opp_signs <= sign_a xor sign_b; -- for having opposite signs, exclusively one of the numbers should be negative, hence the xor

  exp_a_stat <= "00" when  or_vect_bits(exp_a) = '0' else
                "11" when and_vect_bits(exp_a) = '1' else
                "01";

  exp_b_stat <= "00" when  or_vect_bits(exp_b) = '0' else
                "11" when and_vect_bits(exp_b) = '1' else
                "01";

  exp_c_stat <= "00" when  or_vect_bits(exp_c) = '0' else
                "11" when and_vect_bits(exp_c) = '1' else
                "01";

  mnt_a_stat <= "00" when or_vect_bits(mnt_a) = '0' else -- useful for determining if the input is NaN or Infinity 
                "11" when mnt_a(mantissa_size-1) = '1' else -- useful for determining the Type of NaN
                "01";

  mnt_b_stat <= "00" when or_vect_bits(mnt_b) = '0' else -- useful for determining if the input is NaN or Infinity 
                "11" when mnt_b(mantissa_size-1) = '1' else -- useful for determining the Type of NaN
                "01";

  mnt_c_stat <= "00" when or_vect_bits(mnt_c) = '0' else -- useful for determining if the input is NaN or Infinity 
                "11" when mnt_c(mantissa_size-1) = '1' else -- useful for determining the Type of NaN
                "01";

  max_exp_a <= max_exp_a_int;
  max_exp_b <= max_exp_b_int;
  max_exp_c <= max_exp_c_int;

  max_exp_a_int <= exp_a_stat(1);
  max_exp_b_int <= exp_b_stat(1);
  max_exp_c_int <= exp_c_stat(1);

  zero_a <= (exp_a_stat(0) nor exp_a_stat(1)) and (mnt_a_stat(0) nor mnt_a_stat(1));
  zero_b <= (exp_b_stat(0) nor exp_b_stat(1)) and (mnt_b_stat(0) nor mnt_b_stat(1));
  zero_c <= (exp_c_stat(0) nor exp_c_stat(1)) and (mnt_c_stat(0) nor mnt_c_stat(1));

  neg_zero_a <= sign_a and zero_a; -- indicates that the input is a +ve or -ve zero
  neg_zero_b <= sign_b and zero_b; -- indicates that the input is a +ve or -ve zero
  neg_zero_c <= sign_c and zero_c; -- indicates that the input is a +ve or -ve zero

  norm_a <= exp_a_stat(0) xor exp_a_stat(1); -- indicates whether the input is normalized, 1 = normalized, 0 = subnormal
  norm_b <= exp_b_stat(0) xor exp_b_stat(1); -- indicates whether the input is normalized, 1 = normalized, 0 = subnormal
  norm_c <= exp_c_stat(0) xor exp_c_stat(1); -- indicates whether the input is normalized, 1 = normalized, 0 = subnormal

  inf_a <= '1' when max_exp_a = '1' and mnt_a_stat = "00" else '0';
  inf_b <= '1' when max_exp_b = '1' and mnt_b_stat = "00" else '0';
  inf_c <= '1' when max_exp_c = '1' and mnt_c_stat = "00" else '0';

  nan_a <= '1' when max_exp_a = '1' and mnt_a_stat /= "00" else '0';
  nan_b <= '1' when max_exp_b = '1' and mnt_b_stat /= "00" else '0';
  nan_c <= '1' when max_exp_c = '1' and mnt_c_stat /= "00" else '0';

  qnan_a <= '1' when (nan_a and mnt_a(mantissa_size-1)) else '0';
  qnan_b <= '1' when (nan_b and mnt_b(mantissa_size-1)) else '0';
  qnan_c <= '1' when (nan_c and mnt_c(mantissa_size-1)) else '0';

  snan_a <= '1' when (nan_a and not mnt_a(mantissa_size-1)) else '0';
  snan_b <= '1' when (nan_b and not mnt_b(mantissa_size-1)) else '0';
  snan_c <= '1' when (nan_c and not mnt_c(mantissa_size-1)) else '0';

  unsigned_a <= exp_a & mnt_a;  
  unsigned_b <= exp_b & mnt_b;

  comp_a_b <= "10" when unsigned_a > unsigned_b else
              "01" when unsigned_a = unsigned_b else
              "00";

  comp_exp <= "10" when exp_a > exp_b else
              "01" when exp_a = exp_b else
              "00";

end architecture behavioral;
