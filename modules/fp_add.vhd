
-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;

entity fp_add is
  generic (
    size          : natural;
    exponent_size : natural;
    mantissa_size : natural;
    bias          : natural
  );
  port (
  	clk_i             : in  std_logic;
  	rst_ni            : in  std_logic;
    add_sub_en        : in  std_logic;
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
    neg_zero_a        : in  std_logic;
    neg_zero_b        : in  std_logic;
    norm_a            : in  std_logic;
    norm_b            : in  std_logic;
    inf_a             : in  std_logic;
    inf_b             : in  std_logic;
    nan_a             : in  std_logic;
    nan_b             : in  std_logic;
    snan_a            : in  std_logic;
    snan_b            : in  std_logic;
    opp_signs         : in  std_logic; -- indicates whether the signs are equal or opposite
    comp_exp          : in  std_logic_vector(1 downto 0); 
    comp_a_b          : in  std_logic_vector(1 downto 0);
    inexact_add_sub   : out std_logic;
    invalid_add_sub   : out std_logic;
    exp_add_int       : out std_logic_vector(exponent_size-1 downto 0);
    exp_shift_add     : out std_logic_vector(exponent_size downto 0);
    mnt_res_add       : out std_logic_vector(mantissa_size+3 downto 0);
    sign_res_add      : out std_logic; -- contains the sign bit of the outputs
    bypass_add        : out std_logic;
    result_bypass_add : out std_logic_vector(size-1 downto 0);
    ready_add         : out std_logic
  );
end entity fp_add;

architecture floating_point_adder of fp_add is

  --constant exp_shift_ceil    : natural := integer(ceil(log2(real(mantissa_size))));

  signal sign_b_int          : std_logic;
  signal neg_zero_b_int      : std_logic;

  signal exp_a_comp          : std_logic_vector(exponent_size-1 downto 0);
  signal exp_b_comp          : std_logic_vector(exponent_size-1 downto 0);
  signal exp_diff            : std_logic_vector(exponent_size downto 0);
  signal abs_exp_diff        : std_logic_vector(exponent_size downto 0);
  signal exp_sum_int         : std_logic_vector(exponent_size downto 0);
  signal exp_sum             : std_logic_vector(exponent_size downto 0);
  signal max_exp_a           : std_logic;
  signal max_exp_b           : std_logic;
  signal ext_mantissa_a      : std_logic_vector(mantissa_size+2 downto 0); -- extended mantissa
  signal ext_mantissa_b      : std_logic_vector(mantissa_size+2 downto 0); -- extended mantissa
  signal signed_mantissa_a   : std_logic_vector(mantissa_size+3 downto 0);
  signal signed_mantissa_b   : std_logic_vector(mantissa_size+3 downto 0);
  signal aligned_mantissa_a  : std_logic_vector(mantissa_size+3 downto 0);
  signal aligned_mantissa_b  : std_logic_vector(mantissa_size+3 downto 0);
  signal mantissa_norm_int   : std_logic_vector(mantissa_size+3 downto 0);
  signal mantissa_norm       : std_logic_vector(mantissa_size+3 downto 0);

  signal bias_compensate_a   : std_logic;
  signal bias_compensate_b   : std_logic;

  signal opp_signs_int       : std_logic;

  signal unsigned_a          : std_logic_vector(size-2 downto 0);
  signal unsigned_b          : std_logic_vector(size-2 downto 0);

begin

  ready_add <= '1';

  sign_b_int <= (sign_b xor mode); -- the xor will make the sign flip appropriately when the operation is a subtraction

  neg_zero_b_int <= sign_b_int and zero_b; -- '1' when the input is negative zero 

  sign_res_add <= sign_a                      when comp_a_b = "10"     else -- else if a > b, take sign of a
                  sign_b_int                  when comp_a_b = "00"     else -- else if a < b, take sign of b
                  (sign_a and sign_b_int); -- else if a = b take sign of 'a and b' which is always a positive sign unless both inputs are negative 

  opp_signs_int <= opp_signs xor mode;

  invalid_add_sub <= (snan_a or snan_b or (inf_a and inf_b and opp_signs_int)) and add_sub_en;

  MNT_HNDL : process(all)
  begin
    -- Step 1: Extend the Mantissa with one bit being the Implicit MSB, and two bits in the LSB that will help determine if the inexact result is in the middle or above or below the middle.
    ext_mantissa_a     <= norm_a & mnt_a & "00";
    ext_mantissa_b     <= norm_b & mnt_b & "00";
    -- Step 2: Add an extra bit in the MSB, if the mantissas have opposite signs we do twos complement on the smaller number as doing that will make the sum of the mantissa always positive 
    signed_mantissa_a  <= '0' &  ext_mantissa_a; -- default case
    signed_mantissa_b  <= '0' &  ext_mantissa_b; -- default case
    if opp_signs_int = '1' then
      if comp_a_b = "10" then -- if (a > b) flip (b)
        signed_mantissa_b <= std_logic_vector(unsigned(not('0' & ext_mantissa_b(mantissa_size+2 downto 2))) + '1' ) & ext_mantissa_b(1 downto 0); -- two's complement for 'b' since its smaller
      else
        signed_mantissa_a <= std_logic_vector(unsigned(not('0' & ext_mantissa_a(mantissa_size+2 downto 2))) + '1' ) & ext_mantissa_a(1 downto 0); -- two's complement for 'a' if its smaller, and if a=b then we just choose whicever (in this case its a)
      end if;
    end if;

    -- Step 3: Align the mantissas by shifting to the right the mantissa of the input that has the smaller exponent
    aligned_mantissa_a <= signed_mantissa_a; -- default case, take directly the signed mantissa if the numbers have the same exponent
    aligned_mantissa_b <= signed_mantissa_b; -- default case, take directly the signed mantissa if the numbers have the same exponent
    if exp_diff(exponent_size) = '1' then  -- a < b
      aligned_mantissa_a  <= to_stdlogicvector(to_bitvector(signed_mantissa_a) sra to_integer(unsigned(abs_exp_diff))); -- align a
    elsif unsigned(exp_diff(exponent_size-1 downto 0)) = 0 then -- a = b
      null; -- the default case will apply here
    else -- a > b
      aligned_mantissa_b  <= to_stdlogicvector(to_bitvector(signed_mantissa_b) sra to_integer(unsigned(abs_exp_diff))); -- align b
    end if;
    -- Step 4: After aligning, sum the mantissas together
    mnt_res_add <= std_logic_vector(unsigned(aligned_mantissa_a) + unsigned(aligned_mantissa_b));
  end process;

  EXP_HNDL : process(all)
  begin
    bias_compensate_a  <= '0';
    bias_compensate_b  <= '0';
    if norm_a xor norm_b then
      if norm_a = '0' then
        bias_compensate_a <= '1'; -- compensation needed when adding a normalized with a denormalized number as they have different biases
      else
        bias_compensate_b <= '1'; -- compensation needed when adding a normalized with a denormalized number as they have different biases
      end if;
    end if;
    -- Step 1: add the bias compensate to the exponent (bias compensate becomes '1' only when one of the inputs is denormalized) 
    exp_a_comp         <= std_logic_vector(unsigned(exp_a) + bias_compensate_a);
    exp_b_comp         <= std_logic_vector(unsigned(exp_b) + bias_compensate_b);
    -- Step 2: Extend the exponent with an extra sign bit and find the difference between the two exponents 
    exp_diff           <= std_logic_vector(unsigned('0' & exp_a_comp) - unsigned('0' & exp_b_comp));
    -- Step 3: Find the absolute value of the differnce between the two exponents, and let the result take the exponent of the number with the larger exponent, in case they were equal taking either one is fine.
    if exp_diff(exponent_size) = '1' then  -- a < b
      abs_exp_diff        <= std_logic_vector(unsigned(not(exp_diff)) + '1'); -- two's complement to take the absolute
      exp_add_int         <= exp_b_comp;
    elsif unsigned(exp_diff(exponent_size-1 downto 0)) = 0 then -- a = b
      abs_exp_diff        <= exp_diff;
      exp_add_int         <= exp_a_comp;
    else -- a > b
      abs_exp_diff        <= exp_diff;
      exp_add_int         <= exp_a_comp;
    end if;
    -- Step 4: Calculate the shift anount to be done on the exponent from the mantissa to be normalized
    exp_shift_add <= (0 to exponent_size-1 => '0') & mnt_res_add(mantissa_size+2); -- default case if both inputs are denormalized
    if norm_a = '1' or norm_b = '1' then                         
      for i in mantissa_size+3 downto 0 loop
        if mnt_res_add(i) = '1' then
          if i = mantissa_size+3 then
            exp_shift_add <= (1 to exponent_size => '0') & '1'; -- positive shift
            exit;
          elsif i = mantissa_size+2 then
            exp_shift_add <= (others => '0'); -- no shift
            exit;
          else
            exp_shift_add <= std_logic_vector(to_signed(-(mantissa_size+3-i-1),exponent_size+1)); -- negative shift
            exit;
          end if;
        elsif i = 0 then
          exp_shift_add <= '1' & (0 to exponent_size-1 => '0'); -- large shift
        end if;
      end loop;
    end if;
  end process;


  BYPASS_HNDL : process(all)
  begin
    bypass_add <= '0';
    if nan_a or nan_b then -- set the output to the canonical quiet nan (qnan)
      bypass_add <= '1';
      result_bypass_add <= '0' & (0 to exponent_size-1 => '1') & '1' & (mantissa_size-2 downto 0 => '0'); -- The standard does not specify the sign of the NaN, positive sign is chosen for the canonical qNaN
    elsif inf_a xor inf_b then -- if one input is an infinity and the other is not
      bypass_add <= '1';
      if comp_a_b = "10" then -- if a > b, then a is the infinity
        result_bypass_add <= data_a; -- route a to the output
      else
        result_bypass_add <= sign_b_int & data_b(size-2 downto 0); -- route b to the output, with the sign being the sign_b signal which takes into consideration if the operation was a subtraction in order to treat the sign
      end if;
    elsif inf_a and inf_b then -- if both inputs are infinities
      bypass_add <= '1';
      if opp_signs_int then -- if we have a negative infinity and a positve infinity
        result_bypass_add <= '0' & exp_a & '1' & (mantissa_size-2 downto 0 => '0'); -- opp_signs for the sign bit means we take the negative sign
      else
        result_bypass_add <= data_a; -- else we route one of the infinities to the output (in this case we are routing A)
      end if;
    end if;

  end process;

  INEXACT_CHECK : process(all)
  begin
    inexact_add_sub <= '0';
    if (inf_a nor inf_b) then -- if both are not infinity
      if comp_exp = "00" then -- exp_a < exp_b
        if add_vect_bits(ext_mantissa_a) /= 
           add_vect_bits(to_stdlogicvector(to_bitvector(ext_mantissa_a) srl to_integer(unsigned(abs_exp_diff)))) then
          inexact_add_sub <= '1'; -- after shifting, if the count of the bits in the mantissa has decreased (i.e. '1' bits shifted out), then the operation is inexact and requires rounding
        end if;
      elsif comp_exp ="10" then -- exp_a > exo_b
        if add_vect_bits(ext_mantissa_b) /= 
           add_vect_bits(to_stdlogicvector(to_bitvector(ext_mantissa_b) srl to_integer(unsigned(abs_exp_diff)))) then
          inexact_add_sub <= '1'; -- after shifting, if the count of the bits in the mantissa has decreased (i.e. '1' bits shifted out), then the operation is inexact and requires rounding
        end if;
      end if;
    end if;
  end process;

  --process(clk_i, rst_ni)
  --begin
  --  if rst_ni = '0' then
  --  elsif rising_edge(clk_i) then
  --    
  --  end if;
  --end process;

end floating_point_adder;