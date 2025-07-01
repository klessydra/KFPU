-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;


entity fp_conv_f_x is
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
end entity fp_conv_f_x;


architecture Behavioral of fp_conv_f_x is

begin

  
end architecture Behavioral;