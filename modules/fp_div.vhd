-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

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
  clk_i         : in  std_logic;
  rst_ni        : in  std_logic;
  div_en        : in  std_logic;
  op_mode       : in  std_logic_vector(FP_INSTR_LEN-1 downto 0);
  data_a        : in  std_logic_vector(size-1 downto 0);
  data_b        : in  std_logic_vector(size-1 downto 0);
  sign_a        : in  std_logic;
  sign_b        : in  std_logic;
  exp_a         : in  std_logic_vector(exponent_size-1 downto 0);
  exp_b         : in  std_logic_vector(exponent_size-1 downto 0);
  mnt_a         : in  std_logic_vector(mantissa_size-1 downto 0);
  mnt_b         : in  std_logic_vector(mantissa_size-1 downto 0);
  zero_a        : in  std_logic;
  zero_b        : in  std_logic;
  neg_zero_a    : in  std_logic;
  neg_zero_b    : in  std_logic;
  norm_a        : in  std_logic;
  norm_b        : in  std_logic;
  inf_a         : in  std_logic;
  inf_b         : in  std_logic;
  nan_a         : in  std_logic;
  nan_b         : in  std_logic;
  snan_a        : in  std_logic;
  snan_b        : in  std_logic;
  comp_a_b      : in  std_logic_vector(1 downto 0);
  result_div    : out std_logic_vector(size-1 downto 0);
  fp_div_ready  : out std_logic;
  precision_div : out std_logic;
  inexact_div   : out std_logic;
  overflow_div  : out std_logic;
  div_zero      : out std_logic;
  invalid_div   : out std_logic
);
end entity fp_div;

architecture behavioral of fp_div is

  constant mantissa_size_width : integer := integer(ceil(log2(real(mantissa_size+1)))); -- mantissa_size plus the implicit bit

  type DIV_STATES is (UNPACK, NORMALIZE_A, NORMALIZE_B, DIVIDE, DIVIDE_0, DIVIDE_1, DIVIDE_2, DIVIDE_3, NORMALIZE_0,
                 NORMALIZE_1, ROUNDING, PACK);

  signal state : DIV_STATES;

  constant div_op_size : natural := mantissa_size*2+1+4;

  signal i         : integer :=0;
  signal a_m       : std_logic_vector(mantissa_size downto 0); -- one extra implicit bit
  signal a_m2       : std_logic_vector(mantissa_size downto 0); -- one extra implicit bit
  signal b_m       : std_logic_vector(mantissa_size downto 0); -- one extra implicit bit
  signal b_m2       : std_logic_vector(mantissa_size downto 0); -- one extra implicit bit
  signal z_m       : std_logic_vector(mantissa_size+3 downto 0); -- one extra implicit bit
  signal z_m2       : std_logic_vector(mantissa_size+3 downto 0); -- one extra implicit bit
  signal a_e       : std_logic_vector(exponent_size+1 downto 0); -- extended with an overflow bit and a sign bit
  signal b_e       : std_logic_vector(exponent_size+1 downto 0); -- extended with an overflow bit and a sign bit
  signal z_e       : std_logic_vector(exponent_size+1 downto 0); -- extended with an overflow bit and a sign bit
  signal a_s       : std_logic;
  signal b_s       : std_logic;
  signal z_s       : std_logic;
  signal mnt_div_res : std_logic_vector(mantissa_size downto 0);
  signal quotient  : std_logic_vector(div_op_size-1 downto 0) := (others => '0');
  signal divisor   : std_logic_vector(div_op_size-1 downto 0) := (others => '0');
  signal dividend  : std_logic_vector(div_op_size-1 downto 0) := (others => '0');
  signal remainder : std_logic_vector(div_op_size-1 downto 0) := (others => '0');
  signal count     : integer;

  signal round_bit  : std_logic := '0';
  signal sticky     : std_logic := '0';

  signal invalid_div_int : std_logic;
  signal div_start       : std_logic;
  signal mnt_div_done    : std_logic;
  signal div_done_wire   : std_logic;
  signal div_done        : std_logic;

  signal mnt_div        : std_logic_vector(mantissa_size downto 0);
  signal mnt_div2        : std_logic_vector(mantissa_size downto 0);
  signal mnt_clz        : std_logic_vector(mantissa_size_width downto 0);
  signal mnt_clz2        : std_logic_vector(mantissa_size_width downto 0);

  component divider is
    generic (
      divider_implementation : natural;
      size                   : natural
    );
    Port (
      reset                 : in  std_logic;
      clk                   : in  std_logic;
      dividend_i            : in  std_logic_vector(mantissa_size downto 0);
      divisor_i             : in  std_logic_vector(mantissa_size downto 0);
      div_enable            : in  std_logic;
      div_finished          : out std_logic;
      result_div            : out std_logic_vector(mantissa_size downto 0);
      result_rem            : out std_logic_vector(mantissa_size downto 0)
    );
  end component;

  component CLZ_top is
    generic (
      size       : natural;
      size_width : integer
    );
    Port (
      input  : in  std_logic_vector(size-1 downto 0);
      output : out std_logic_vector(size_width downto 0)
    );
  end component;

begin


  divider_inst : divider
    generic map(
      divider_implementation => 5,
      size                   => mantissa_size+1
    )
    port map(
      reset        => not rst_ni,
      clk          => clk_i,
      dividend_i   => a_m2, 
      divisor_i    => b_m2,
      div_enable   => div_start,
      div_finished => mnt_div_done,
      result_div   => mnt_div_res,
      result_rem   => open
    );

  CLZ_Inst : CLZ_top
    generic map(
      size       => mantissa_size+1,
      size_width => mantissa_size_width
    )
    port map (
      input  => mnt_div,
      output => mnt_clz
    );

  CLZ_Inst2 : CLZ_top
    generic map(
      size       => mantissa_size+1,
      size_width => mantissa_size_width
    )
    port map (
      input  => mnt_div2,
      output => mnt_clz2
    );


  --fp_div_ready <= '1' when state = UNPACK else '0';

  process(clk_i, rst_ni) 
  begin
    if rst_ni = '0' then
      state       <= UNPACK;
  	  precision_div <= '0';
      inexact_div   <= '0';
      round_bit   <= '0';
      sticky      <= '0';
      quotient    <= (others => '0');
      divisor     <= (others => '0');
      dividend    <= (others => '0');
      remainder   <= (others => '0');
      count       <=  0; 
      invalid_div <= '0';
      div_zero    <= '0';
      div_done    <= '0';
      overflow_div <= '0';
      div_start    <= '0';
    elsif rising_edge(clk_i) then
      div_done     <= div_done_wire;
      invalid_div  <= '0';
      div_zero     <= '0';
      overflow_div <= '0';
      div_start    <= '0';

      case state is

        when UNPACK => 
          a_m   <= norm_a & mnt_a;
          b_m   <= norm_b & mnt_b;
          a_m2   <= norm_a & mnt_a;
          b_m2   <= norm_b & mnt_b;
          a_e   <= std_logic_vector(unsigned("00" & exp_a) - bias);
          b_e   <= std_logic_vector(unsigned("00" & exp_b) - bias);
          a_s   <= sign_a;
          b_s   <= sign_b;
          count     <=  0; 
          precision_div <= '0';
          inexact_div   <= '0';
          round_bit <= '0';
          sticky    <= '0';
          state     <= UNPACK;

          if div_en then
            -- if 'a' is NaN or 'b' is NaN return NaN
            if nan_a or nan_b then
              result_div  <= '0' & (0 to exponent_size-1 => '1') & '1' & (0 to mantissa_size-2 => '0');
              state       <= UNPACK;
              invalid_div <= snan_a or snan_b;
            -- else if 'a' is infinity and 'b' is inf return NaN
            elsif inf_a and inf_b then
              result_div  <= '0' & (0 to exponent_size-1 => '1') & '1' & (0 to mantissa_size-2 => '0');
              state       <= UNPACK;
              invalid_div <= '1';
            -- else if 'a' is infinity return infinity only if 'b' is not infinity
            elsif inf_a then
              result_div <= (sign_a xor sign_b) & (0 to exponent_size-1 => '1') & (0 to mantissa_size-1 => '0');
              state      <= UNPACK;
            -- else if 'b' is inf return zero
            elsif inf_b then
              result_div <= (sign_a xor sign_b) & (0 to size-2 => '0');
              state      <= UNPACK;
            -- else if 'a' is zero return zero only if 'b' is not zero
            elsif zero_a then
              result_div <= (sign_a xor sign_b) & (0 to size-2 => '0');
              -- if 'b' is zero return NaN
              if zero_b then
                result_div <= '0' & (0 to exponent_size-1 => '1') & '1' & (0 to mantissa_size-2 => '0');
                invalid_div <= '1';
              end if;
              state    <= UNPACK;
            -- else if 'b' is zero return inf
            elsif zero_b then
              result_div <= (sign_a xor sign_b) & (0 to exponent_size-1 => '1') & (0 to mantissa_size-1 => '0');
              state      <= UNPACK;
              div_zero  <= not inf_a; -- division by zero flag is not raised if 'a' is infinity
            -- else if 'a' is equal to 'b'
            elsif comp_a_b = "01" then
              result_div <= (sign_a xor sign_b) & (0 to exponent_size-1 => std_logic_vector(to_unsigned(bias,exponent_size))) & (0 to mantissa_size-1 => '0');
              state      <= UNPACK;
            else
              -- denormalized number
              if (norm_a = '0') then
                a_e <= std_logic_vector(to_signed(-bias+1, exponent_size+2));
              end if;
              -- denormalized number
              if (norm_b = '0') then
                b_e <= std_logic_vector(to_signed(-bias+1, exponent_size+2));
              end if;
              div_start <= '1';
              if not a_m(mantissa_size) then -- if 'b' is not normalized, then normalize it 
                state <= NORMALIZE_A;
              elsif not b_m(mantissa_size) then -- if 'b' is not normalized, then normalize it 
                state <= NORMALIZE_B;
              else
                state <= DIVIDE_0;
              end if;
            end if;
          end if;

        when NORMALIZE_A =>
            a_m <= std_logic_vector(unsigned(a_m) sll to_integer(unsigned(mnt_clz)));
            a_e <= std_logic_vector(unsigned(a_e) - to_integer(unsigned(mnt_clz)));
            if not b_m(mantissa_size) then -- if 'b' is not normalized, then normalize it 
              state <= NORMALIZE_B;
            else
              state <= DIVIDE_0;
            end if;

        when NORMALIZE_B =>
          state <= DIVIDE_0;
          b_m <= std_logic_vector(unsigned(b_m) sll to_integer(unsigned(mnt_clz)));
          b_e <= std_logic_vector(unsigned(b_e) - to_integer(unsigned(mnt_clz)));

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
          z_m2       <= mnt_div_res & (0 to 2 => '0');
          z_m        <= quotient(mantissa_size+3 downto 1) & (quotient(0) or or_vect_bits(remainder));
          precision_div <= quotient(2);
          round_bit <= quotient(1);
          sticky    <= quotient(0) or or_vect_bits(remainder);
          state     <= NORMALIZE_0;

        when NORMALIZE_0 =>
          z_m2       <= z_m2 sll to_integer(unsigned(mnt_clz2));      
          if (z_m(mantissa_size+3) = '0' and signed(z_e) > -bias+1) then
            z_e       <= std_logic_vector(unsigned(z_e) - 1);
            z_m       <= z_m sll to_integer(unsigned(mnt_clz));
            precision_div <= round_bit;
            round_bit <= '0';
          else
            state <= NORMALIZE_1;
          end if;

        when NORMALIZE_1 =>
          if (signed(z_e) < -bias+1) then
            z_e       <= std_logic_vector(unsigned(z_e) + 1);
            z_m2       <= '0' & z_m2(mantissa_size+3 downto 1);
            z_m       <= '0' & z_m(mantissa_size+3 downto 1);
            precision_div <= z_m(3);
            round_bit <= z_m(2);
            sticky    <= sticky or z_m(1);
          else
            inexact_div <= round_bit or sticky;
            state       <= PACK;
            --state       <= ROUNDING;
          end if;

        --when ROUNDING =>
        --  if (precision_div and (inexact_div or z_m(0))) then
        --    z_m <= std_logic_vector(unsigned(z_m) + 1);
        --    if (z_m = (0 to mantissa_size => '1')) then
        --      z_e <= std_logic_vector(unsigned(z_e) + 1);
        --    end if;
        --  end if;
        --  state <= PACK;

        when PACK =>
          result_div <= z_s & std_logic_vector(unsigned(z_e(exponent_size-1 downto 0)) + bias) & z_m(mantissa_size+2 downto 3);
          if (signed(z_e) = -bias+1 and z_m(mantissa_size+3) = '0') then
            result_div(size-2 downto size-exponent_size-1) <= (others => '0');
          end if;
          -- if overflow occurs, return inf
          if (signed(z_e) > bias) then
            precision_div <= '1';
            overflow_div  <= '1';
            result_div <= z_s & (1 to exponent_size-1 => '1') & '0' & (0 to mantissa_size-1 => '1');
          end if;
          state <= UNPACK;

        when others =>
          state <= UNPACK;

      end case;
    end if;
  end process;

  fp_div_ready <= div_done when op_mode(3) else '1';

  process(all) 
  begin
    div_done_wire <= div_done;
    mnt_div       <= a_m;
    mnt_div2      <= z_m2(mantissa_size+3 downto 3);

    case state is

      when UNPACK => 
        div_done_wire <= div_en;
        if div_en then
          if nan_a or nan_b then
          elsif inf_a and inf_b then
          elsif inf_a then
          elsif inf_b then
          elsif zero_a then
          elsif zero_b then
          elsif comp_a_b = "01" then
          else
            div_done_wire <= '0';
          end if;
        end if;

      when NORMALIZE_A =>

      when NORMALIZE_B =>
        mnt_div <= b_m;

      when DIVIDE_0 =>           

      when DIVIDE_1 =>

      when DIVIDE_2 =>

      when DIVIDE_3 =>

      when NORMALIZE_0 =>
        mnt_div <= z_m(mantissa_size+3 downto 3);

      when NORMALIZE_1 =>

      when ROUNDING =>

      when PACK =>
        div_done_wire <= '1';

      when others =>

    end case;
  end process;

end architecture behavioral;

