-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;

entity fp_div is
generic (
  size          : natural;
  exponent_size : natural;
  mantissa_size : natural;
  bias          : natural
);
port(
  clk_i      : in  std_logic;
  rst_ni     : in  std_logic;
  valid_i    : in  std_logic;
  div_en     : in  std_logic;
  data_a     : in  std_logic_vector(size-1 downto 0);
  data_b     : in  std_logic_vector(size-1 downto 0);
  sign_a     : in  std_logic;
  sign_b     : in  std_logic;
  exp_a      : in  std_logic_vector(exponent_size-1 downto 0);
  exp_b      : in  std_logic_vector(exponent_size-1 downto 0);
  mnt_a      : in  std_logic_vector(mantissa_size-1 downto 0);
  mnt_b      : in  std_logic_vector(mantissa_size-1 downto 0);
  zero_a     : in  std_logic;
  zero_b     : in  std_logic;
  neg_zero_a : in  std_logic;
  neg_zero_b : in  std_logic;
  norm_a     : in  std_logic;
  norm_b     : in  std_logic;
  inf_a      : in  std_logic;
  inf_b      : in  std_logic;
  nan_a      : in  std_logic;
  nan_b      : in  std_logic;
  comp_a_b   : in  std_logic_vector(1 downto 0);
  result_div : out std_logic_vector(size-1 downto 0);
  div_ready  : out std_logic;
  div_done   : out std_logic
);
end entity fp_div;

architecture behavioral of fp_div is

  type DIV_STATES is (UNPACK, NORMALIZE_A, NORMALIZE_B, DIVIDE_0, DIVIDE_1, DIVIDE_2, DIVIDE_3, NORMALIZE_0,
                 NORMALIZE_1, ROUNDING, PACK);

  signal state : DIV_STATES;

  constant div_op_size : natural := mantissa_size*2+1+4;

  signal i         : integer :=0;
  signal a_m       : std_logic_vector(mantissa_size downto 0); -- one extra implicit bit
  signal b_m       : std_logic_vector(mantissa_size downto 0); -- one extra implicit bit
  signal z_m       : std_logic_vector(mantissa_size downto 0); -- one extra implicit bit
  signal a_e       : std_logic_vector(exponent_size+1 downto 0); -- extended with an overflow bit and a sign bit
  signal b_e       : std_logic_vector(exponent_size+1 downto 0); -- extended with an overflow bit and a sign bit
  signal z_e       : std_logic_vector(exponent_size+1 downto 0); -- extended with an overflow bit and a sign bit
  signal a_s       : std_logic;
  signal b_s       : std_logic;
  signal z_s       : std_logic;
  signal quotient  : std_logic_vector(div_op_size-1 downto 0) := (others => '0');
  signal divisor   : std_logic_vector(div_op_size-1 downto 0) := (others => '0');
  signal dividend  : std_logic_vector(div_op_size-1 downto 0) := (others => '0');
  signal remainder : std_logic_vector(div_op_size-1 downto 0) := (others => '0');
  signal count     : integer;

  signal guard      : std_logic := '0';
  signal round_bit  : std_logic := '0';
  signal sticky     : std_logic := '0';

begin

  --div_ready <= '1' when state = UNPACK else '0';

  process(clk_i, rst_ni) 
  begin
    if rst_ni = '0' then
      state     <= UNPACK;
  	  guard     <= '0';
      round_bit <= '0';
      sticky    <= '0';
      quotient  <= (others => '0');
      divisor   <= (others => '0');
      dividend  <= (others => '0');
      remainder <= (others => '0');
      count     <= 0; 
    elsif rising_edge(clk_i) then
      div_done <= '0';

      case state is

        when UNPACK => 
          a_m   <= norm_a & mnt_a;
          b_m   <= norm_b & mnt_b;
          a_e   <= std_logic_vector(unsigned("00" & exp_a) - bias);
          b_e   <= std_logic_vector(unsigned("00" & exp_b) - bias);
          a_s   <= sign_a;
          b_s   <= sign_b;
          div_done  <= '0';
          count     <= 0; 
          guard     <= '0';
          round_bit <= '0';
          sticky    <= '0';
          state     <= UNPACK;

          if div_en = '1' then
            -- if 'a' is NaN or 'b' is NaN return NaN
            if nan_a or nan_b then
              result_div <= '1' & (0 to exponent_size-1 => '1') & '1' & (0 to mantissa_size-2 => '0');
              state      <= UNPACK;
              div_done  <= '1';
            -- else if 'a' is inf and 'b' is inf return NaN
            elsif inf_a and inf_b then
              result_div <= '1' & (0 to exponent_size-1 => '1') & '1' & (0 to mantissa_size-2 => '0');
              state     <= UNPACK;
              div_done <= '1';
            -- else if 'a' is inf return inf only if 'b' is not zero
            elsif inf_a then
              result_div <= (sign_a xor sign_b) & (0 to exponent_size-1 => '1') & (0 to mantissa_size-1 => '0');
              state <= UNPACK;
              div_done <= '1';
            -- else if 'b' is inf return zero
            elsif inf_b then
              result_div <= (sign_a xor sign_b) & (0 to size-2 => '0');
              state      <= UNPACK;
            -- else if 'a' is zero return zero only if 'b' is not zero
            elsif zero_a then
              result_div <= (sign_a xor sign_b) & (0 to size-2 => '0');
              -- if 'b' is zero return NaN
              if zero_b then
                result_div <= '1' & (0 to exponent_size-1 => '1') & '1' & (0 to mantissa_size-2 => '0');                   
              end if;
              state     <= UNPACK;
              div_done <= '1';
            -- else if 'b' is zero return inf
            elsif zero_b then
              result_div <= (sign_a xor sign_b) & (0 to exponent_size-1 => '1') & (0 to mantissa_size-1 => '0');
              state      <= UNPACK;
              div_done  <= '1';
            -- else if 'a' is equal to 'b'
            elsif comp_a_b = "01" then
              result_div <= (sign_a xor sign_b) & (0 to exponent_size-1 => std_logic_vector(to_unsigned(bias,exponent_size))) & (0 to mantissa_size-1 => '0');
              state      <= UNPACK;
              div_done  <= '1';
            else
              -- denormalized number
              if (norm_a = '0') then
                a_e <= std_logic_vector(to_signed(-bias+1, exponent_size+2));
              end if;
              -- denormalized number
              if (norm_b = '0') then
                b_e <= std_logic_vector(to_signed(-bias+1, exponent_size+2));
              end if;
              if (div_en) then
                state     <= NORMALIZE_A;
              else
                state <= UNPACK;
                div_done <= '1';
              end if;
            end if;
          end if;

        when NORMALIZE_A =>
          if a_m(mantissa_size) then
            state <= NORMALIZE_B;
          else
            a_m <= std_logic_vector(unsigned(a_m) sll 1);
            a_e <= std_logic_vector(unsigned(a_e) - 1);
          end if;

        when NORMALIZE_B =>
          if b_m(mantissa_size) then
            state <= DIVIDE_0;
          else
            b_m <= std_logic_vector(unsigned(b_m) sll 1);
            b_e <= std_logic_vector(unsigned(b_e) - 1);
          end if;

        when DIVIDE_0 =>
          z_s <= a_s xor b_s;
          z_e <= std_logic_vector(unsigned(a_e) - unsigned(b_e));
          quotient  <= (others => '0');
          remainder <= (others => '0');
          count     <= 0;
          dividend  <= a_m & (0 to mantissa_size+3 => '0');
          divisor   <= (0 to mantissa_size+3 => '0') & b_m;
          state     <= DIVIDE_1;            

        when DIVIDE_1 =>
          quotient  <= quotient(div_op_size-2 downto 0) & '0';
          remainder <= remainder(div_op_size-2 downto 0) & dividend(div_op_size-1);
          dividend  <= dividend(div_op_size-2 downto 0) & '0';
          state     <= DIVIDE_2;

        when DIVIDE_2 =>
          if (remainder >= divisor) then
            quotient(0) <= '1';
            remainder   <= std_logic_vector(unsigned(remainder) - unsigned(divisor));
          end if;
          if (count = div_op_size-2) then
            state <= DIVIDE_3;
          else
            count <= count + 1;
            state <= DIVIDE_1;
          end if;

        when DIVIDE_3 =>
          z_m       <= quotient(mantissa_size+3 downto 3);
          guard     <= quotient(2);
          round_bit <= quotient(1);
          sticky    <= quotient(0) or or_vect_bits(remainder);
          state     <= NORMALIZE_0;

        when NORMALIZE_0 =>
          if (z_m(mantissa_size) = '0' and signed(z_e) > -bias+1) then
            z_e       <= std_logic_vector(unsigned(z_e) - 1);
            z_m       <= z_m(mantissa_size-1 downto 0) & guard;
            guard     <= round_bit;
            round_bit <= '0';
          else
            state <= NORMALIZE_1;
          end if;

        when NORMALIZE_1 =>
          if (signed(z_e) < -bias+1) then
            z_e       <= std_logic_vector(unsigned(z_e) + 1);
            z_m       <= '0' & z_m(mantissa_size downto 1);
            guard     <= z_m(0);
            round_bit <= guard;
            sticky    <= sticky or round_bit;
          else
            state     <= ROUNDING;
          end if;

        when ROUNDING =>
          if (guard and ((round_bit or sticky) or z_m(0))) then
            z_m <= std_logic_vector(unsigned(z_m) + 1);
            if (z_m = (0 to mantissa_size => '1')) then
              z_e <= std_logic_vector(unsigned(z_e) + 1);
            end if;
          end if;
          state <= PACK;

        when PACK =>
          result_div <= z_s & std_logic_vector(unsigned(z_e(exponent_size-1 downto 0)) + bias) & z_m(mantissa_size-1 downto 0);
          if (signed(z_e) = -bias+1 and z_m(mantissa_size) = '0') then
            result_div(size-2 downto size-exponent_size-1) <= (others => '0');
          end if;
          -- if overflow occurs, return inf
          if (signed(z_e) > bias) then
            result_div <= z_s & (0 to exponent_size-1 => '1') & (0 to mantissa_size-1 => '0');
          end if;
          state <= UNPACK;
          div_done <= '1';

        when others =>
          state <= UNPACK;

      end case;
    end if;
  end process;


  process(all) 
  begin
    div_ready <= '0';

    case state is

      when UNPACK => 
        div_ready <= '1';

        if div_en = '1' then
          if nan_a or nan_b then
          elsif inf_a and inf_b then
          elsif inf_a then
          elsif inf_b then
          elsif zero_a then
          elsif zero_b then
          elsif comp_a_b = "01" then
          else
            if (div_en) then
              div_ready <= not(valid_i);
            else
            end if;
          end if;
        end if;

      when NORMALIZE_A =>

      when NORMALIZE_B =>

      when DIVIDE_0 =>           

      when DIVIDE_1 =>

      when DIVIDE_2 =>

      when DIVIDE_3 =>

      when NORMALIZE_0 =>

      when NORMALIZE_1 =>

      when ROUNDING =>

      when PACK =>
        div_ready <= '1';

      when others =>

    end case;
  end process;

end architecture behavioral;

