-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;

entity fp_sqrt is 
  generic (
    size           : natural;
    exponent_size  : natural;
    mantissa_size  : natural;
    bias           : natural
  );
  port (
    clk_i              : in  std_logic;
    rst_ni             : in  std_logic;
    sqrt_en            : in  std_logic;
    op_mode            : in  std_logic_vector(FP_INSTR_LEN-1 downto 0);
    data_a             : in  std_logic_vector(size-1 downto 0); 
    sign_a             : in  std_logic;
    exp_a              : in  std_logic_vector(exponent_size-1 downto 0);
    mnt_a              : in  std_logic_vector(mantissa_size-1 downto 0);
    zero_a             : in  std_logic;
    neg_zero_a         : in  std_logic;
    norm_a             : in  std_logic;
    inf_a              : in  std_logic;
    nan_a              : in  std_logic;
    snan_a             : in  std_logic;
    qnan_a             : in  std_logic;
    fp_sqrt_ready      : out std_logic;
    fp_sqrt_res        : out std_logic_vector(size-1 downto 0);
    bypass_sqrt        : out std_logic;
    result_bypass_sqrt : out std_logic_vector(size-1 downto 0);
    precision_sqrt     : out std_logic;
    inexact_sqrt       : out std_logic;
    invalid_sqrt       : out std_logic
  );
end entity fp_sqrt;

architecture behavioural of fp_sqrt is

  signal mnt_lat        : unsigned(mantissa_size+1 downto 0);
  signal radicand       : unsigned(mantissa_size+1 downto 0);
  signal adj_denrom_mnt : unsigned(mantissa_size+1 downto 0);
  signal adj_radicand   : unsigned(mantissa_size+1 downto 0);
  signal x05_2          : unsigned((mantissa_size+2)*2-1 downto 0) := (others => '0'); -- This signal represents x.5^2

  signal adj_exp        : natural;
  signal odd_exp        : natural;
  signal a              : unsigned(exponent_size-1 downto 0);
  signal b              : natural;

  signal sqrt_start     : std_logic := '0';
  signal sqrt_busy      : std_logic;
  signal sqrt_ready     : std_logic;
  signal sqrt_res       : std_logic_vector(mantissa_size+1 downto 0);

  -- Signals for the output
  signal exp_result     : std_logic_vector(exponent_size-1 downto 0);
  signal mnt_result     : std_logic_vector(mantissa_size downto 0);

  signal precision_sqrt_int : std_logic;
  signal inexact_sqrt_int   : std_logic;
  signal bypass_sqrt_int    : std_logic;

  -- Square root Newton Raphson
  component sqrt is
    generic (
      sqrt_implementation : natural;
      size                : natural;
      fraction_size       : natural
    );
    Port (
      clk_i          : in  std_logic;
      rst_ni         : in  std_logic;
      start          : in  std_logic;
      number         : in  std_logic_vector(size-1 downto 0); -- Input number as std_logic_vector
      sqrt_res       : out std_logic_vector(size-1 downto 0); -- Output square root as std_logic_vector
      busy           : out std_logic;
      ready          : out std_logic;
      precision_sqrt : out std_logic;  -- Indicates result is exactly in the middle
      inexact_sqrt   : out std_logic  -- Indicates the result is not exact
    );
  end component;

begin

  sqrt_start     <= sqrt_en            and not (sqrt_busy or bypass_sqrt_int);
  precision_sqrt <= precision_sqrt_int and not bypass_sqrt_int;
  inexact_sqrt   <= inexact_sqrt_int   and not bypass_sqrt_int;

  -- Instantiate the square root component
  sqrt_inst : sqrt
    generic map(
      sqrt_implementation => 0,
      size                => mantissa_size+2,
      fraction_size       => mantissa_size
    )
    port map(
      clk_i          => clk_i,
      rst_ni         => rst_ni,
      start          => sqrt_start,
      number         => std_logic_vector(radicand), -- the mantissa and the implicit bit
      sqrt_res       => sqrt_res,
      busy           => sqrt_busy,
      ready          => sqrt_ready,
      precision_sqrt => precision_sqrt_int,
      inexact_sqrt   => inexact_sqrt_int
    );

  process(all)
    variable i_rev : natural;
  begin
    adj_denrom_mnt <= unsigned('0' & '0' & mnt_a);
    adj_exp <= 0;
    i_rev   := 0;
    for i in mantissa_size-1 downto 0 loop
      if mnt_a(i) then
        if (i_rev mod 2)=0 then
          adj_denrom_mnt <= unsigned(mnt_a(i downto 0) & (mantissa_size-1 downto i => '0')) & '0';
          adj_exp <= i_rev;
          exit;
        else
          adj_denrom_mnt <= '0' & unsigned(mnt_a(i downto 0) & (mantissa_size-1 downto i => '0'));
          adj_exp <= i_rev;
          exit;
        end if;
      end if;
      i_rev := i_rev+1;
    end loop;
  end process;

  adj_radicand   <= adj_denrom_mnt                 when norm_a = '0' else 
                    unsigned('0' & norm_a & mnt_a) when odd_exp = 1  else
                    unsigned(norm_a & mnt_a & '0');
  --adj_radicand   <= unsigned('0' & mnt_a & '0') when norm_a = '0' else unsigned('0' & norm_a & mnt_a);
  radicand       <= adj_radicand when sqrt_start else mnt_lat;
  --x05_2          <= unsigned(sqrt_res & '1')*unsigned(sqrt_res & '1');
  fp_sqrt_ready  <= sqrt_ready;

  -- Processing results and handling special cases
  process(rst_ni, clk_i)
  begin
    if (rst_ni = '0') then
      mnt_lat <= (others => '0');
    elsif rising_edge(clk_i) then
      if (sqrt_start) then
        mnt_lat <= adj_radicand;
      end if;
    end if;
  end process;

  RES_BYPASS : process(all)
  begin
    invalid_sqrt <= '0';
    bypass_sqrt_int  <= '0';
    result_bypass_sqrt <= data_a; -- route data_a to the output (result for an infinity input)
    if op_mode(4) then
      if (zero_a) then -- sqrt of zero is zero and sqrt of negative zero is negative zero
        bypass_sqrt_int <= '1';
        result_bypass_sqrt <= sign_a & (0 to size-2 => '0');
      elsif sign_a or nan_a then -- set the output to the canonical quiet nan (qnan) for negative and input NaNs
        bypass_sqrt_int <= '1';
        result_bypass_sqrt <= '0' & (0 to exponent_size-1 => '1') & '1' & (mantissa_size-2 downto 0 => '0'); -- The IEEE standard does not specify the sign of the NaN, positive sign is chosen for the canonical qNaN
        invalid_sqrt <= snan_a or (sign_a and not qnan_a);
      elsif inf_a then -- if one input is an infinity and the other is not
        bypass_sqrt_int <= '1';
      end if;
    end if;
  end process;

  odd_exp     <= to_integer(unsigned(exp_a(0 downto 0)));
  a           <= unsigned(exp_a)/2 + bias/2;
  b           <= odd_exp;
  -- Calculate the new exponent
  mnt_result  <= sqrt_res(mantissa_size downto 0);  -- Get mantissa result from sqrt unit
  exp_result  <= std_logic_vector(a + b) when norm_a else std_logic_vector(to_unsigned((bias)/2-adj_exp/2, exponent_size)); -- Adjust exponent for sqrt
  --exp_result  <= std_logic_vector(a + b) when norm_a else std_logic_vector(to_unsigned((bias/2)-1, exponent_size)+sqrt_res(mantissa_size));

  fp_sqrt_res <= sign_a & exp_result & mnt_result(mantissa_size-1 downto 0);
  bypass_sqrt <= bypass_sqrt_int;

end behavioural;