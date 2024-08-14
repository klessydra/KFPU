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
    clk_i       : in  std_logic;
    rst_ni      : in  std_logic;
    valid_i     : in  std_logic;
    data_i      : in  std_logic_vector(size-1 downto 0); 
    sign_a      : in  std_logic;
    exp_a       : in  std_logic_vector(exponent_size-1 downto 0);
    mnt_a       : in  std_logic_vector(mantissa_size-1 downto 0);
    zero_a      : in  std_logic;
    neg_zero_a  : in  std_logic;
    norm_a      : in  std_logic;
    inf_a       : in  std_logic;
    nan_a       : in  std_logic;
    valid_o     : out std_logic;
    result_sqrt : out std_logic_vector(size-1 downto 0);
    precision   : out std_logic;
    inexact     : out std_logic
  );
end entity fp_sqrt;

architecture behavioural of fp_sqrt is

  signal exp_part      : std_logic_vector(exponent_size downto 0);
  signal mnt_tmp       : std_logic_vector(mantissa_size downto 0);
  signal mnt_tmp2      : std_logic_vector(mantissa_size downto 0);
  signal mnt_shift     : std_logic_vector(mantissa_size downto 0);
  signal mnt_extended  : std_logic_vector(mantissa_size+2 downto 0);
  signal mnt_shifted   : std_logic_vector(mantissa_size+2 downto 0);
  signal mnt_sqrt      : std_logic_vector(mantissa_size downto 0);
  signal sqrt_approx   : std_logic_vector(mantissa_size-1 downto 0);

  signal part_done  : std_logic := '0';
  signal count      : integer := 2; 
  signal result     : std_logic_vector(4 downto 0) := "00000"; 
  signal partialq   : std_logic_vector(5 downto 0) := "000000";

  signal busy      : std_logic := '0';

  signal sqrt_done : std_logic := '0';
  signal r_data    : std_logic_vector(mantissa_size+2 downto 0) := (others => '0');
  signal r_sqrt    : std_logic_vector(mantissa_size+2 downto 0) := (others => '0');
  signal mnt_res   : std_logic_vector(mantissa_size+2 downto 0) := (others => '0');
  signal start_sqrt: std_logic := '0';

begin

  mnt_extended <= norm_a & mnt_a & "00";
  mnt_tmp  <= '1' & mnt_a when norm_a else mnt_a & '0';
  mnt_tmp2 <= '0' & mnt_tmp(mantissa_size downto 1) when exp_a(0) else mnt_tmp;

  process(all)
  begin
    -- shift left by even bits until msb is 1x or 01
    mnt_shift <= mnt_tmp2;
    if (norm_a = '0') then
      for i in mantissa_size-2 downto 0 loop
        if mnt_tmp2(i) = '1' then
          if (mantissa_size-i) mod 2 = 0 then -- because "mantissa_size-i" is even, we shift by "mantissa_size-i"  
            mnt_shift <= std_logic_vector(unsigned(mnt_tmp2) sll (mantissa_size-i));
          else -- else "mantissa_size-i" is odd, so we shift by "mantissa_size-(i+1)" in order to maintain an even shift to the left
            mnt_shift <= std_logic_vector(unsigned(mnt_tmp2) sll (mantissa_size-(i+1))) ;
          end if;
        end if;
      end loop;
    end if;
  end process;

  process(all)
  begin
    if (norm_a = '1') then -- if normalized
      if (exp_a(1)) then   -- if the exponent is odd
        mnt_shifted <= std_logic_vector(unsigned(mnt_extended) srl 2);
      else -- if the exponent is even
        mnt_shifted <= std_logic_vector(unsigned(mnt_extended) srl 1);
      end if;
      exp_part <= std_logic_vector(unsigned('0' & exp_a) + bias + exp_a(0));
    else
      if (mnt_extended(mantissa_size+1) or mnt_extended(mantissa_size)) then
        mnt_shifted <= mnt_extended;
      else
        for i in mantissa_size-1 downto 0 loop
          if (i = 1) then
            mnt_shifted <= std_logic_vector(unsigned(mnt_extended) sll i);
            exit;
          end if;
        end loop;
      end if;
    end if;
    if (sqrt_done) then
      mnt_res <= r_sqrt(mantissa_size+1 downto 0) & '0';
    end if;
  end process;

  --process(clk_i, mnt_shifted, part_done)  
  --begin
  --  if rising_edge(clk_i) then
  --    count <= 2;
  --    part_done <= '0';
  --    if (valid_i or busy) then
  --      busy <= '1';
  --      if(part_done = '0')then
  --        if(count >= 0) then
  --          partialq(1 downto 0)  <= mnt_shifted((count * 2) + 1 downto count * 2);
  --          part_done <= '1';
  --        else
  --          busy <= '0';
  --          mnt_sqrt <= result(3 downto 0);
  --        end if;    
  --        count <= count - 1;
  --      elsif(part_done = '1') then
  --        if((result(3 downto 0) & "01") <= partialq) then
  --          result   <= result(3 downto 0) & '1';
  --          partialq(5 downto 2) <= std_logic_vector(unsigned(partialq(3 downto 0)) - unsigned(result(1 downto 0) & "01"));    
  --        else 
  --          result <= result(3 downto 0) & '0';
  --          partialq(5 downto 2) <= partialq(3 downto 0);                     
  --        end if;   
  --        part_done  <= '0';
  --      end if;
  --    end if;
  --  end if;  
  --end process;   

  process (clk_i, rst_ni)
  begin
    if rst_ni = '0' then
      sqrt_done  <= '0';
      start_sqrt <= '0';
      r_data     <= (others => '0');
      r_sqrt     <= (others => '0');
    elsif rising_edge(clk_i) then
      sqrt_done  <= start_sqrt;
      if valid_i = '1' then
        r_data <= "00" & mnt_shifted(mantissa_size+2 downto 2);
        start_sqrt <= '1';
        sqrt_done  <= '0';
      end if;
      if start_sqrt = '1' then
        for i in 0 to mantissa_size+3 loop
          if (i*i <= unsigned(r_data)) and ((i+1)*(i+1) > unsigned(r_data)) then
            r_sqrt <= std_logic_vector(to_unsigned(i, mantissa_size+3));
            start_sqrt <= '0';
            sqrt_done  <= '1';
            exit;
          end if;
        end loop;
      end if;
    end if;
  end process;


end behavioural;