
-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;

entity fp_cmp is
  generic (
    size          : natural;
    exponent_size : natural;
    mantissa_size : natural;
    bias          : natural
  );
  port (
    clk_i             : in  std_logic;
    rst_ni            : in  std_logic;
    op_mode           : in  std_logic_vector(FP_INSTR_LEN-1 downto 0);
    sub_op_mode       : in  std_logic_vector(FP_SUB_OP_LEN-1 downto 0);
    data_a            : in  std_logic_vector(size-1 downto 0);
    data_b            : in  std_logic_vector(size-1 downto 0);
    sign_a            : in  std_logic;
    sign_b            : in  std_logic;
    opp_signs         : in  std_logic;
    comp_a_b          : in  std_logic_vector(1 downto 0);
    zero_a            : in  std_logic;
    zero_b            : in  std_logic;
    nan_a             : in  std_logic;
    nan_b             : in  std_logic;
    snan_a            : in  std_logic;
    snan_b            : in  std_logic;
    res_cmp           : out std_logic;
    res_min_max       : out std_logic_vector(size-1 downto 0);
    invalid_cmp       : out std_logic
  );
end entity fp_cmp;

architecture Behavioral of fp_cmp is

  signal a_eq_b : std_logic;
  signal a_le_b : std_logic;
  signal a_lt_b : std_logic;
  signal a_ne_b : std_logic;
  signal a_gt_b : std_logic;
  signal a_ge_b : std_logic;

  signal invalid_qnan_en : std_logic;

begin

  invalid_qnan_en <= op_mode(6) and (sub_op_mode(1) or sub_op_mode(3) or sub_op_mode(5) or sub_op_mode(7)); -- input QNANs are consiered invalid for FLT, FLE, FGT, FGE 
  invalid_cmp     <= ((snan_a or snan_b) or ((nan_a or nan_b) and invalid_qnan_en))  and (op_mode(6) or op_mode(7));

  res_cmp <= (a_eq_b and sub_op_mode(0)) or -- equal
             (a_le_b and sub_op_mode(1)) or -- less than or equal
             (a_lt_b and sub_op_mode(3)) or -- less than
             (a_ne_b and sub_op_mode(4)) or -- not equal
             (a_gt_b and sub_op_mode(5)) or -- greater than
             (a_ge_b and sub_op_mode(7));   -- greater than or equal

  process(all)
  begin
    res_min_max <= (others => '0');
    a_eq_b      <= '0';
    a_gt_b      <= '0';
    a_ge_b      <= '0';
    a_lt_b      <= '0';
    a_le_b      <= '0';
    a_ne_b      <= not(a_eq_b);
    if not(nan_a) and not(nan_b) then
      if comp_a_b = "01" then -- a = b
        if opp_signs = '0' then
          res_min_max <= data_a;
          a_eq_b <= '1';
          a_ge_b <= '1';
          a_le_b <= '1';
        elsif sign_a then
          if sub_op_mode(0) then -- max
            res_min_max <= data_b;
          else -- min
            res_min_max <= data_a;
          end if;
          a_le_b <= '1';
          a_lt_b <= '1';
        else -- sign_b
          if sub_op_mode(0) then -- max
            res_min_max <= data_a;
          else -- min
            res_min_max <= data_b;
          end if;
          a_ge_b <= '1';
          a_gt_b <= '1';
        end if;
      elsif comp_a_b = "10" then -- abs(a) > abs(b)
        if sign_a = '0' then -- if a > 0
          if sub_op_mode(0) then -- max
            res_min_max <= data_a;
          else -- min
            res_min_max <= data_b;
          end if;
          a_gt_b <= '1';
          a_ge_b <= '1';
        else -- if a < 0
          if sub_op_mode(0) then -- max
            res_min_max <= data_b;
          else -- min
            res_min_max <= data_a;
          end if;
          a_lt_b <= '1';
          a_le_b <= '1';
        end if;
      elsif comp_a_b = "00" then -- abs(a) < abs(b)
        if sign_b = '0' then -- if b > 0
          if sub_op_mode(0) then -- max
            res_min_max <= data_b;
          else -- min
            res_min_max <= data_a;
          end if;
          a_lt_b <= '1';
          a_le_b <= '1';
        else -- if a < 0
          if sub_op_mode(0) then -- max
            res_min_max <= data_a;
          else -- min
            res_min_max <= data_b;
          end if;
          a_gt_b <= '1';
          a_ge_b <= '1';
        end if;
      end if;
    else
      if nan_a and nan_B then
        res_min_max <= '0' & (0 to exponent_size-1 => '1') & '1' & (0 to mantissa_size-2 => '0'); -- if SNAN set 
      elsif nan_a then
        res_min_max <= data_b; -- take B if A is a NAN
      else -- nan_b
        res_min_max <= data_a; -- take A if B is a NAN
      end if;
    end if;
    if zero_a and zero_b then -- considered equal even if one is negative zero and the other is positive zero
      a_eq_b <= '1';
      a_ge_b <= '1';
      a_le_b <= '1';
      a_lt_b <= '0'; -- overwrites any '1' assignments made previously
      a_gt_b <= '0'; -- overwrites any '1' assignments made previously
    end if;
  end process;

end architecture Behavioral;