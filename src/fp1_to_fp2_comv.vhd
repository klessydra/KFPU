-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;


entity fp1_to_fp2_conv is
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
    inexact_conv      : out std_logic;
    exp_o             : out std_logic_vector(exponent_size-1 downto 0);
    mnt_o             : out std_logic_vector(mantissa_size+3 downto 0);
    sign_o            : out std_logic;
    ready_conv        : out std_logic
  );
end entity fp1_to_fp2_conv;


architecture Behavioral of fp1_to_fp2_conv is
  -- Internal signals
  signal internal_exp   : std_logic_vector(exponent_size-1 downto 0);
  signal internal_mnt   : std_logic_vector(mantissa_size+3 downto 0);
  signal internal_sign  : std_logic;
  signal conversion_done: std_logic;

begin
  process(clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      -- Reset internal signals
      internal_exp   <= (others => '0');
      internal_mnt   <= (others => '0');
      internal_sign  <= '0';
      conversion_done<= '0';
      exp_o          <= (others => '0');
      mnt_o          <= (others => '0');
      sign_o         <= '0';
      ready_conv     <= '0';
      inexact_conv   <= '0';
      
    elsif rising_edge(clk_i) then
      if valid_i = '1' then
        -- Implement conversion logic here
        
        -- Example of setting internal signals
        internal_exp   <= exp_a;
        internal_mnt   <= mnt_a & "0000";  -- Simple example of mantissa extension
        internal_sign  <= sign_a;
        
        -- Set output signals after conversion
        exp_o          <= internal_exp;
        mnt_o          <= internal_mnt;
        sign_o         <= internal_sign;
        ready_conv     <= '1';
        inexact_conv   <= '0';  -- Example, set accordingly to the conversion logic
        
        conversion_done<= '1';
      else
        ready_conv     <= '0';
        conversion_done<= '0';
      end if;
    end if;
  end process;
  
end architecture Behavioral;
