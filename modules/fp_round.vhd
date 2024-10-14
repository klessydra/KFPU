-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;

entity fp_round is 
  generic (
    size          : natural := 8;
    exponent_size : natural := 4;
    mantissa_size : natural := 3
  );
  port(
    clk_i           : in  std_logic;
    rst_ni          : in  std_logic;
    op_mode         : in  std_logic_vector(FP_INSTR_LEN-1 downto 0);
    round_mode      : in  std_logic_vector(7 downto 0);
    result_ru       : in  std_logic_vector(size-1 downto 0);
    overflow_norm   : in  std_logic;
    inexact_round   : in  std_logic;
    precision_round : in  std_logic;
    opp_signs       : in  std_logic;
    comp_a_b        : in  std_logic_vector(1 downto 0);
    result          : out std_logic_vector(size-1 downto 0)
  );
end entity fp_round;

architecture behavioral of fp_round is

  signal inexact        : std_logic;
  signal precision      : std_logic;
  signal even           : std_logic;
  signal result_rounded : std_logic_vector(size-1 downto 0);

begin

  inexact   <= inexact_round;
  precision <= precision_round;

  even <= result_ru(0);

  ROUNDING: process(all)
  begin

    if result_ru(size-2 downto mantissa_size) = (0 to exponent_size-1 => '1') then
      result_rounded <= result_ru;
    else
      if round_mode(RNE_bit_position) = '1' then
        if inexact = '1' then -- not on the edges, and not in the middle
          if precision = '1' then -- above the middle
            result_rounded <= std_logic_vector(unsigned(result_ru)+1);
          else -- below the middle
            result_rounded <= result_ru;
          end if;
        else
          if precision = '1' then --right in the middle
            if even = '0' then -- if the mantissa of the current results is even (i.e. LSB is is 0)
              result_rounded <= result_ru;
            else -- else if the mantisaa is odd, then round to the number above
              result_rounded <= std_logic_vector(unsigned(result_ru)+1); -- this type of integer addition is fine,since the exponent is to the left of the mantissa, and an overflow in the mantissa will result in automatically incrementing the exponent 
            end if;
          else
            result_rounded <= result_ru;
          end if;
        end if;
      elsif round_mode(RTZ_bit_position) = '1' then
        result_rounded <= result_ru;
      elsif round_mode(RDN_bit_position) = '1' then
        if (comp_a_b(0) and (opp_signs xor op_mode(1)) and (op_mode(0) or op_mode(1))) then -- result is set negaative zero instead of zero when inputs are equal but their signs are ooposite, and its an add or a sub operation
          result_rounded <= '1' & (0 to size-2 => '0');
        else
          if result_ru(size-1) = '1' then
            if precision = '1' or inexact = '1' then
              result_rounded <= std_logic_vector(unsigned(result_ru)+1);
            else 
              result_rounded <= result_ru;
            end if;
          else 
            result_rounded <= result_ru;
          end if;
        end if;
      elsif round_mode(RUP_bit_position) = '1' then
        if result_ru(size-1) = '0' then
          if precision = '1' or inexact = '1' then
            result_rounded <= std_logic_vector(unsigned(result_ru)+1);
          else 
            result_rounded <= result_ru;
          end if;
        else 
          result_rounded <= result_ru;
        end if;
      elsif round_mode(RMM_bit_position) = '1' then
        result_rounded <= std_logic_vector(unsigned(result_ru)+precision);
      else

      end if;
    end if;
  end process;

  result <= result_rounded;

end behavioral;

