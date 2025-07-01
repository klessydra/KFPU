-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;


entity fp_conv_f_f is
  generic (
    size          : natural;
    exponent_size : natural;
    mantissa_size : natural;
    bias          : natural
  );
  port (
    clk_i             : in  std_logic;
    rst_ni            : in  std_logic;
    valid_i           : in  std_logic;
    mode              : in  std_logic;
    data_a            : in  std_logic_vector(size-1 downto 0);
    sign_a            : in  std_logic;
    exp_a             : in  std_logic_vector(exponent_size-1 downto 0);
    mnt_a             : in  std_logic_vector(mantissa_size-1 downto 0);
    zero_a            : in  std_logic;
    neg_zero_a        : in  std_logic;
    norm_a            : in  std_logic;
    inf_a             : in  std_logic;
    nan_a             : in  std_logic;
    inexact_conv_f_f  : out std_logic;
    res_conv_f_f      : out std_logic_vector(63 downto 0); -- the converted result my fit inside a 64-bit output
    ready_conv        : out std_logic
  );
end entity fp_conv_f_f;


architecture Behavioral of fp_conv_f_f is

  constant size_2           : natural := 8;
  constant exponent_size_2  : natural := 5;
  constant mantissa_size_2  : natural := 7;
  constant bias_2           : integer := 15;

  signal internal_exp    : std_logic_vector(exponent_size-1 downto 0);
  signal internal_mnt    : std_logic_vector(mantissa_size+3 downto 0);
  signal internal_sign   : std_logic;
  signal conversion_done : std_logic;

  signal exp_res_conv    : integer;
  signal mnt_res_conv    : std_logic_vector(mantissa_size_2-1 downto 0);

begin

  process(all)
  begin
    exp_res_conv <= to_integer(unsigned(exp_a)) - bias + bias_2;
    if mantissa_size_2 > mantissa_size then
      mnt_res_conv <= mnt_a & (0 to mantissa_size_2 - mantissa_size - 1 => '0');
    end if;
    --if (zero_a) then
    --  exp_res_conv <= 0;
    --  mnt_res_conv <= (others => '0');
    --elsif (nan_a) then
    --  exp_res_conv <= 0;
    --  mnt_res_conv <= '1' & (0 to size_2-2 => '0');
    --elsif (unsigned(exp_a) - bias < -(bias_2 - 1 + mantissa_size_2)) then -- the number in A is smaller then the smallest denorm in B
    --  exp_res_conv <= 0;
    --  mnt_res_conv <= (others => '0');
    --  inexact_conv_f_f <= '1';
    --elsif (unsigned(exp_a) - bias < -(bias_2 - 1)) then -- the number in A is a denorm in B 
    --  exp_res_conv <= 0;
    --  mnt_res_conv(0) <= '0';
    --  mnt_res_conv(mantissa_size_2-1 downto 3) <= (others => '0');
    --  inexact_conv_f_f <= '1';
    --end if;
    if mantissa_size_2 > mantissa_size then
    else
    end if; 
  end process;

  process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      -- Reset internal signals
      internal_exp       <= (others => '0');
      internal_mnt       <= (others => '0');
      internal_sign      <= '0';
      conversion_done    <= '0';
      ready_conv         <= '0';
      
    elsif rising_edge(clk_i) then
      if valid_i = '1' then
        -- Implement conversion logic here

        -- Example of setting internal signals
        internal_exp   <= exp_a;
        internal_mnt   <= mnt_a & "0000";  -- Simple example of mantissa extension
        internal_sign  <= sign_a;

        -- Set output signals after conversion
        ready_conv       <= '1';
        
        conversion_done  <= '1';
      else
        ready_conv       <= '0';
        conversion_done  <= '0';
      end if;
    end if;
  end process;
  
end architecture Behavioral;