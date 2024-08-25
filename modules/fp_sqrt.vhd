-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;


entity fp_sqrt is 
  generic (
    size          : natural;
    exponent_size : natural;
    mantissa_size : natural;
    bias          : natural
  );
  port( 
    clk         : in  std_logic;
    clrn        : in  std_logic;
    d           : in  std_logic_vector(31 downto 0); 
    rm          : in  std_logic_vector(1 downto 0);
    fsqrt       : in  std_logic;
    ena         : in  std_logic;
    s           : out std_logic_vector(31 downto 0);
    reg_x       : out std_logic_vector(25 downto 0);
    count       : out std_logic_vector(4 downto 0);
    busy        : out std_logic;
    stall       : out std_logic
  );

end entity fp_sqrt;

architecture behavioural of fp_sqrt is

  constant ZERO                          : std_logic_vector(31 downto 0) := (others => '0');
  constant INF                           : std_logic_vector(31 downto 0) := x"7f800000";
  constant NaN                           : std_logic_vector(31 downto 0) := x"7fc00000";
  signal d_expo_is_00                    : std_logic;
  signal d_expo_is_ff                    : std_logic;
  signal d_frac_is_00                    : std_logic;
  signal sign_a                          : std_logic;
  signal exp_8                           : std_logic_vector(7 downto 0);
  signal d_f24                           : std_logic_vector(23 downto 0);
  signal d_temp24                        : std_logic_vector(23 downto 0);
  signal d_frac24                        : std_logic_vector(23 downto 0);
  signal shamt_d                         : std_logic_vector(4 downto 0);
  signal exp0                            : std_logic_vector(7 downto 0);
  signal e1_sign, e1_e00, e1_eff, e1_f00 : std_logic;
  signal e2_sign, e2_e00, e2_eff, e2_f00 : std_logic;
  signal e3_sign, e3_e00, e3_eff, e3_f00 : std_logic;
  signal e1_rm, e2_rm, e3_rm             : std_logic_vector(1 downto 0);
  signal e1_exp, e2_exp, e3_exp          : std_logic_vector(7 downto 0);
  signal frac0                           : std_logic_vector(31 downto 0);
  signal frac                            : std_logic_vector(26 downto 0);
  signal frac_plus_1                     : std_logic;
  signal frac_rnd                        : std_logic_vector(24 downto 0);
  signal expo_new                        : std_logic_vector(7 downto 0);
  signal frac_new                        : std_logic_vector(22 downto 0);

  component shift_even_bits
    generic (
      size          : natural;
      exponent_size : natural;
      mantissa_size : natural;
      bias          : natural
    );
    port (
      a  : in  std_logic_vector(23 downto 0);
      b  : out std_logic_vector(23 downto 0);
      sa : out  std_logic_vector(4 downto 0)
    );
  end component;

  component root_newton24
    generic (
      size          : natural;
      exponent_size : natural;
      mantissa_size : natural;
      bias          : natural
    );
    port (
      clk      : in  std_logic;
      clrn     : in  std_logic;
      ena      : in  std_logic;
      fsqrt    : in  std_logic;
      d_frac24 : in  std_logic_vector(23 downto 0);
      frac0    : out std_logic_vector(31 downto 0);
      busy     : out std_logic;
      count    : out std_logic_vector(4 downto 0);
      reg_x    : out std_logic_vector(25 downto 0);
      stall    : out std_logic
    );
  end component;

function final_result(
  d_sign : std_logic;
  d_e00  : std_logic;
  d_eff  : std_logic;
  d_f00  : std_logic;
  calc   : std_logic_vector(31 downto 0)
) return std_logic_vector is
  variable result   : std_logic_vector(31 downto 0);
  variable combined : std_logic_vector(3 downto 0); -- Temporary variable to hold concatenation
begin
  -- Combine the signals into a std_logic_vector
  combined := d_sign & d_e00 & d_eff & d_f00;

  case unsigned(combined) is  -- Cast to unsigned for case evaluation
    when "1XXX" => -- Match "1xxx"
      result := NaN;  -- Assign NaN
    when "000X" => -- Match "000x"
      result := calc; -- Assign calc
    when "0100" => -- Match "0100"
      result := calc; -- Assign calc
    when "0010" => -- Match "0010"
      result := NaN;  -- Assign NaN
    when "0011" => -- Match "0011"
      result := INF;  -- Assign INF
    when others =>
      result := ZERO; -- Assign ZERO
  end case;

  return result;
end function;

begin

  d_expo_is_00 <= not(or_vect_bits(d(30 downto 23)));
  d_expo_is_ff <= and_vect_bits(d(30 downto 23));
  d_frac_is_00 <= not(or_vect_bits(d(22 downto 0)));

  sign_a   <= d(31);
  exp_8    <= std_logic_vector(unsigned('0' & d(30 downto 24)) + 63 + d(23));
  d_f24    <= (d(22 downto 0) & '0') when d_expo_is_00 else ('1' & d(22 downto 0));
  d_temp24 <= ('0' & d_f24(23 downto 1)) when d(23) else d_f24;
  exp0     <= std_logic_vector(unsigned(exp_8) - unsigned("0000" & shamt_d(4 downto 1)));

  process(clrn, clk)
  begin
    if(not(clrn)) then
      e1_rm   <= (others => '0'); e2_rm <= (others => '0'); e3_rm <= (others => '0');
      e1_exp  <= (others => '0'); e2_exp  <= (others => '0'); e3_exp  <= (others => '0');
      e1_sign <= '0'; e2_sign <= '0'; e3_sign <= '0';
      e1_e00  <= '0'; e2_e00  <= '0'; e3_e00  <= '0';
      e1_eff  <= '0'; e2_eff  <= '0'; e3_eff  <= '0';
      e1_f00  <= '0'; e2_f00  <= '0'; e3_f00  <= '0';
    elsif rising_edge(clk) then
      if (ena) then
        e1_sign <= sign_a; e2_sign      <= e1_sign; e3_sign <= e2_sign;
        e1_rm   <= rm;     e2_rm        <= e1_rm;   e3_rm   <= e2_rm;
        e1_exp  <= exp0;   e2_exp       <= e1_exp;  e3_exp  <= e2_exp;
        e1_e00  <= d_expo_is_00; e2_e00 <= e1_e00;  e3_e00  <= e2_e00;
        e1_eff  <= d_expo_is_ff; e2_eff <= e1_eff;  e3_eff  <= e2_eff;
        e1_f00  <= d_frac_is_00; e2_f00 <= e1_f00; e3_f00   <= e2_f00;
      end if;
    end if;
  end process;

  shift_d : shift_even_bits
    generic map(
      size          => size,
      exponent_size => exponent_size,
      mantissa_size => mantissa_size,
      bias          => bias 
    )
    port map(
      a  => d_temp24,
      b  => d_frac24,
      sa => shamt_d
    );

  frac_newton : root_newton24
    generic map(
      size          => size,
      exponent_size => exponent_size,
      mantissa_size => mantissa_size,
      bias          => bias
    )
    port map(
      clk      => clk,
      clrn     => clrn,
      ena      => ena,
      fsqrt    => fsqrt,
      d_frac24 => d_frac24,
      frac0    => frac0,
      busy     => busy,
      count    => count,
      reg_x    => reg_x,
      stall    => stall
    );

  frac <= frac0(31 downto 6) & or_vect_bits(frac0(5 downto 0));
  frac_plus_1 <=
    (not e3_rm(1) and not e3_rm(0) and  frac(3) and  frac(2) and not frac(1) and not frac(0)) or
    (not e3_rm(1) and not e3_rm(0) and  frac(2) and (frac(1) or frac(0)))                     or
    (not e3_rm(1) and     e3_rm(0) and (frac(2) or   frac(1) or frac(0)) and     e3_sign)     or
    (    e3_rm(1) and not e3_rm(0) and (frac(2) or   frac(1) or frac(0)) and not e3_sign);

  frac_rnd <= std_logic_vector(unsigned('0' & frac(26 downto 3)) + frac_plus_1);

  expo_new <= std_logic_vector(unsigned(e3_exp) + 1) when frac_rnd(24) else e3_exp;
  frac_new <= frac_rnd(23 downto 1)                  when frac_rnd(24) else frac_rnd(22 downto 0);
  s        <= final_result(e3_sign, e3_e00, e3_eff, e3_f00, (e3_sign & expo_new & frac_new));

end behavioural;

-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;

entity shift_even_bits is
  generic (
    size          : natural;
    exponent_size : natural;
    mantissa_size : natural;
    bias          : natural
  );
  port (
    a   : in  std_logic_vector(23 downto 0);
    b   : out std_logic_vector(23 downto 0);
    sa  : out std_logic_vector(4 downto 0)
  );
end entity shift_even_bits;


architecture behavioural of shift_even_bits is
  signal a5, a4, a3, a2, a1 : std_logic_vector(23 downto 0);
begin

  a5    <= a;
  sa(4) <= not or_vect_bits(a5(23 downto 8));
  a4    <= (others => '0') when sa(4) = '1' else a5(7 downto 0)  & (8 to 23 => '0');
  sa(3) <= not or_vect_bits(a4(23 downto 16));
  a3    <= (others => '0') when sa(3) = '1' else a4(15 downto 0) & (16 to 23 => '0');
  sa(2) <= not or_vect_bits(a3(23 downto 20));
  a2    <= (others => '0') when sa(2) = '1' else a3(19 downto 0) & (20 to 23 => '0');
  sa(1) <= not or_vect_bits(a2(23 downto 22));
  a1    <= (others => '0') when sa(1) = '1' else a2(21 downto 0) & (22 to 23 => '0');
  sa(0) <= '0';
  b     <= a1;

end behavioural;