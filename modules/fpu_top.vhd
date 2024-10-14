
-- ieee packages ------------
library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- local packages ------------
use work.fpu_pkg.all;

entity fpu_top is  
  generic (
    size          : natural := 8;
    exponent_size : natural := 4;
    mantissa_size : natural := 3;
    bias          : natural := 7
  );
  port (
    clk_i            : in  std_logic;
    rst_ni           : in  std_logic;
    op_mode          : in  std_logic_vector(FP_INSTR_LEN-1 downto 0);
    round_mode       : in  std_logic_vector(7 downto 0);
    data_a           : in  std_logic_vector(size-1 downto 0);
    data_b           : in  std_logic_vector(size-1 downto 0);
    data_c           : in  std_logic_vector(size-1 downto 0);
    valid_i          : in  std_logic;
    result           : out std_logic_vector(size-1 downto 0);
    div_ready        : out std_logic;
    fp_div_ready     : out std_logic;
    fp_sqrt_ready    : out std_logic;
    flag_nx          : out std_logic;
    flag_uf          : out std_logic;
    flag_of          : out std_logic;
    flag_dz          : out std_logic;
    flag_in          : out std_logic
  );
end entity fpu_top;

architecture behavioral of fpu_top is

  signal sign_a              : std_logic;
  signal sign_b              : std_logic;
  signal sign_c              : std_logic;
  signal exp_a               : std_logic_vector(exponent_size-1 downto 0);
  signal exp_b               : std_logic_vector(exponent_size-1 downto 0);
  signal exp_c               : std_logic_vector(exponent_size-1 downto 0);
  signal mnt_a               : std_logic_vector(mantissa_size-1 downto 0);
  signal mnt_b               : std_logic_vector(mantissa_size-1 downto 0);
  signal mnt_c               : std_logic_vector(mantissa_size-1 downto 0);
  signal zero_a              : std_logic;
  signal zero_b              : std_logic;
  signal zero_c              : std_logic;
  signal neg_zero_a          : std_logic;
  signal neg_zero_b          : std_logic;
  signal neg_zero_c          : std_logic;
  signal norm_a              : std_logic;
  signal norm_b              : std_logic;
  signal norm_c              : std_logic;
  signal max_exp_a           : std_logic;
  signal max_exp_b           : std_logic;
  signal max_exp_c           : std_logic;
  signal inf_a               : std_logic;
  signal inf_b               : std_logic;
  signal inf_c               : std_logic;
  signal nan_a               : std_logic;
  signal nan_b               : std_logic;
  signal nan_c               : std_logic;
  signal qnan_a              : std_logic;
  signal qnan_b              : std_logic;
  signal qnan_c              : std_logic;
  signal snan_a              : std_logic;
  signal snan_b              : std_logic;
  signal snan_c              : std_logic;
  signal opp_signs           : std_logic;
  signal comp_exp            : std_logic_vector(1 downto 0); 
  signal comp_a_b            : std_logic_vector(1 downto 0);

  signal result_int          : std_logic_vector(size-1 downto 0);

  signal mode                : std_logic;
  signal div_zero            : std_logic;
  signal invalid_add_sub     : std_logic;
  signal invalid_mul         : std_logic;
  signal invalid_div         : std_logic;
  signal invalid_sqrt        : std_logic;
  signal exp_add_int         : std_logic_vector(exponent_size-1 downto 0);
  signal exp_mul_int         : std_logic_vector(exponent_size+1 downto 0);
  signal exp_int             : std_logic_vector(exponent_size+1 downto 0);
  signal exp_shift_add       : std_logic_vector(exponent_size downto 0);
  signal exp_shift_mul       : std_logic_vector(exponent_size downto 0);
  signal exp_shift           : std_logic_vector(exponent_size downto 0);
  signal mnt_res_add         : std_logic_vector(mantissa_size+3 downto 0);
  signal mnt_res_mul         : std_logic_vector((mantissa_size+1)*2+1 downto 0);
  signal sign_res_add        : std_logic;
  signal sign_res_mul        : std_logic;
  signal sign_res_div        : std_logic := '0';
  signal sign_res_sqrt       : std_logic := '0';
  signal sign_res_fma        : std_logic := '0';
  signal sign_res            : std_logic;
  signal bypass_add          : std_logic;
  signal bypass_mul          : std_logic;
  signal bypass_div          : std_logic := '0';
  signal bypass_sqrt         : std_logic := '0';
  signal bypass_fma          : std_logic := '0';
  signal bypass              : std_logic;
  signal result_bypass_add   : std_logic_vector(size-1 downto 0);
  signal result_bypass_mul   : std_logic_vector(size-1 downto 0);
  signal result_bypass_div   : std_logic_vector(size-1 downto 0);
  signal result_bypass_sqrt  : std_logic_vector(size-1 downto 0);
  signal result_bypass_fma   : std_logic_vector(size-1 downto 0);
  signal result_bypass       : std_logic_vector(size-1 downto 0);
  signal result_ru           : std_logic_vector(size-1 downto 0);
  signal result_ru_int       : std_logic_vector(size-1 downto 0);

  signal result_div          : std_logic_vector(size-1 downto 0);
  signal fp_sqrt_res         : std_logic_vector(size-1 downto 0);

  signal precision_div       : std_logic;
  signal precision_sqrt      : std_logic;
  signal precision_round     : std_logic;
  signal precision_round_int : std_logic;
  signal inexact_add_sub     : std_logic;
  signal inexact_mul         : std_logic;
  signal inexact_div         : std_logic;
  signal inexact_sqrt        : std_logic;
  signal inexact_norm        : std_logic;
  signal inexact_round       : std_logic;
  signal inexact_round_int   : std_logic;

  signal uf_3_trig          : std_logic;

  signal ready_add          : std_logic;
  signal ready_mul          : std_logic;

  signal underflow_mul      : std_logic;
  signal underflow          : std_logic;
  signal overflow_mul       : std_logic;
  signal overflow_div       : std_logic;
  signal overflow_norm      : std_logic;
  signal overflow           : std_logic;

  signal data_a_lat         : std_logic_vector(size-1 downto 0);

  signal add_sub_en         : std_logic;
  signal mul_en             : std_logic;
  signal div_en             : std_logic;
  signal sqrt_en            : std_logic;
  signal sqrt_ready         : std_logic;

  signal max_exp_in         : std_logic;
  signal max_exp_out        : std_logic;
  signal min_exp_out        : std_logic;
  signal min_mnt_ru         : std_logic;
  signal max_mnt_ru         : std_logic;
  signal min_exp_ru         : std_logic;
  signal max_exp_ru         : std_logic;
  signal min_mnt_out        : std_logic;
  signal zero_out           : std_logic;
  signal denorm_out         : std_logic;
  signal round_norm_out     : std_logic;
  signal inf_out            : std_logic;
  signal flag_of_int        : std_logic;
  signal flag_in_int        : std_logic;

  component fp_classify
    generic (
      size          : natural;
      exponent_size : natural;
      mantissa_size : natural
    );
    port (
      clk_i            : in  std_logic;
      rst_ni           : in  std_logic;
      data_a           : in  std_logic_vector(size-1 downto 0);
      data_b           : in  std_logic_vector(size-1 downto 0);
      data_c           : in  std_logic_vector(size-1 downto 0);
      sign_a           : out std_logic;
      sign_b           : out std_logic;
      sign_c           : out std_logic;
      exp_a            : out std_logic_vector(exponent_size-1 downto 0);
      exp_b            : out std_logic_vector(exponent_size-1 downto 0);
      exp_c            : out std_logic_vector(exponent_size-1 downto 0);
      mnt_a            : out std_logic_vector(mantissa_size-1 downto 0);
      mnt_b            : out std_logic_vector(mantissa_size-1 downto 0);
      mnt_c            : out std_logic_vector(mantissa_size-1 downto 0);
      zero_a           : out std_logic;
      zero_b           : out std_logic;
      zero_c           : out std_logic;
      neg_zero_a       : out std_logic;
      neg_zero_b       : out std_logic;
      neg_zero_c       : out std_logic;
      norm_a           : out std_logic;
      norm_b           : out std_logic;
      norm_c           : out std_logic;
      max_exp_a        : out std_logic;
      max_exp_b        : out std_logic;
      max_exp_c        : out std_logic;
      inf_a            : out std_logic;
      inf_b            : out std_logic;
      inf_c            : out std_logic;
      nan_a            : out std_logic;
      nan_b            : out std_logic;
      nan_c            : out std_logic;
      qnan_a           : out std_logic;
      qnan_b           : out std_logic;
      qnan_c           : out std_logic;
      snan_a           : out std_logic;
      snan_b           : out std_logic;
      snan_c           : out std_logic;
      opp_signs        : out std_logic;
      comp_exp         : out std_logic_vector(1 downto 0); 
      comp_a_b         : out std_logic_vector(1 downto 0)
    );
  end component;

  component fp_add
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
      opp_signs         : in  std_logic;
      comp_exp          : in  std_logic_vector(1 downto 0); 
      comp_a_b          : in  std_logic_vector(1 downto 0);
      inexact_add_sub   : out std_logic;
      invalid_add_sub   : out std_logic;
      exp_add_int       : out std_logic_vector(exponent_size-1 downto 0);
      exp_shift_add     : out std_logic_vector(exponent_size downto 0);
      mnt_res_add       : out std_logic_vector(mantissa_size+3 downto 0);
      sign_res_add      : out std_logic;
      bypass_add        : out std_logic;
      result_bypass_add : out std_logic_vector(size-1 downto 0);
      ready_add         : out std_logic
    );
  end component;

  component fp_mul
    generic (
      size          : natural;
      exponent_size : natural;
      mantissa_size : natural;
      bias          : natural
    );
    port(
      clk_i             : in  std_logic;
      rst_ni            : in  std_logic;
      mul_en            : in  std_logic;
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
      norm_a            : in  std_logic;
      norm_b            : in  std_logic;
      inf_a             : in  std_logic;
      inf_b             : in  std_logic;
      nan_a             : in  std_logic;
      nan_b             : in  std_logic;
      snan_a            : in  std_logic;
      snan_b            : in  std_logic;
      opp_signs         : in  std_logic;
      comp_a_b          : in  std_logic_vector(1 downto 0);
      precision         : out std_logic;
      inexact_mul       : out std_logic;
      invalid_mul       : out std_logic;
      exp_mul_int       : out std_logic_vector(exponent_size+1 downto 0);
      exp_shift_mul     : out std_logic_vector(exponent_size downto 0);
      mnt_res_mul       : out std_logic_vector((mantissa_size+1)*2+1 downto 0);
      sign_res_mul      : out std_logic;
      bypass_mul        : out std_logic;
      result_bypass_mul : out std_logic_vector(size-1 downto 0);
      underflow_mul     : out std_logic;
      overflow_mul      : out std_logic;
      ready_mul         : out std_logic
    );
  end component;

  component fp_div
    generic (
      size          : natural;
      exponent_size : natural;
      mantissa_size : natural;
      bias          : natural
    );
    port (
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
  end component;

  component fp_sqrt
    generic (
      size          : natural;
      exponent_size : natural;
      mantissa_size : natural;
      bias          : natural
    );
    port( 
      clk_i              : in  std_logic;
      rst_ni             : in  std_logic;
      sqrt_en            : in  std_logic;
      op_mode            : in  std_logic_vector(FP_INSTR_LEN-1 downto 0);
      data_a             : in  std_logic_vector(size-1 downto 0); 
      sign_a             : in  std_logic;
      exp_a              : in  std_logic_vector(exponent_size-1 downto 0);
      mnt_a              : in  std_logic_vector(mantissa_size-1 downto 0);
      zero_a             : in  std_logic;
      neg_zero_a         : in  std_logic;
      norm_a             : in  std_logic;
      inf_a              : in  std_logic;
      nan_a              : in  std_logic;
      snan_a             : in  std_logic;
      qnan_a             : in  std_logic;
      fp_sqrt_ready      : out std_logic;
      fp_sqrt_res        : out std_logic_vector(size-1 downto 0);
      bypass_sqrt        : out std_logic;
      result_bypass_sqrt : out std_logic_vector(size-1 downto 0);
      precision_sqrt     : out std_logic;
      inexact_sqrt       : out std_logic;
      invalid_sqrt       : out std_logic
    );
  end component;

  component fp_norm
    generic (
      size            : natural;
      exponent_size   : natural;
      mantissa_size   : natural;
      bias            : natural
    );
    port( 
      clk_i           : in  std_logic;
      rst_ni          : in  std_logic;
      op_mode         : in  std_logic_vector(FP_INSTR_LEN-1 downto 0);
      round_mode      : in  std_logic_vector(7 downto 0);
      norm_a          : in  std_logic;
      norm_b          : in  std_logic;
      inf_a           : in  std_logic;
      inf_b           : in  std_logic;
      nan_a           : in  std_logic;
      nan_b           : in  std_logic;
      comp_exp        : in  std_logic_vector(1 downto 0); 
      comp_a_b        : in  std_logic_vector(1 downto 0);
      inexact_norm    : in  std_logic;
      exp_int         : in  std_logic_vector(exponent_size+1 downto 0);
      exp_shift       : in  std_logic_vector(exponent_size downto 0);
      mnt_res_add     : in  std_logic_vector(mantissa_size+3 downto 0);
      mnt_res_mul     : in  std_logic_vector((mantissa_size+1)*2+1 downto 0);
      sign_res        : in  std_logic;
      bypass          : in  std_logic;
      result_bypass   : in  std_logic_vector(size-1 downto 0);
      underflow       : in  std_logic;
      uf_3_trig       : out std_logic;
      overflow_norm   : out std_logic;
      precision_round : out std_logic;
      inexact_round   : out std_logic;
      result_ru       : out std_logic_vector(size-1 downto 0)
    );
  end component;


  component fp_round 
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
      precision_round : in  std_logic;
      inexact_round   : in  std_logic;
      opp_signs       : in  std_logic;
      comp_a_b        : in  std_logic_vector(1 downto 0);
      result          : out std_logic_vector(size-1 downto 0)
    );
  end component;

begin

  flag_nx <= (inexact_round_int or precision_round_int or flag_of_int) and not flag_in_int and not max_exp_in and not div_zero;
  flag_uf <= ((zero_out or denorm_out or round_norm_out) and (inexact_round_int or precision_round_int));
  flag_of <= flag_of_int;
  flag_dz <= div_zero;
  flag_in <= flag_in_int;

  flag_of_int <= (inf_out and not max_exp_in and not div_zero) or (overflow_norm and not bypass and not op_mode(3)) or overflow_div;
  flag_in_int <= invalid_add_sub or invalid_mul or invalid_div or invalid_sqrt;

  mode <= '1' when op_mode(1) = '1' else '0';

  zero_out            <= min_exp_out and     min_mnt_out;
  denorm_out          <= min_exp_out and not min_mnt_out;
  -- indicates that the denormalized result was rounded to a normalized result
  round_norm_out      <= max_mnt_ru and min_exp_ru and min_mnt_out and uf_3_trig;
  min_mnt_ru          <= not(or_vect_bits(result_ru(mantissa_size-1 downto 0)));
  max_mnt_ru          <= and_vect_bits(result_ru(mantissa_size-1 downto 0));
  min_exp_ru          <= not(or_vect_bits(result_ru(mantissa_size+exponent_size-1 downto mantissa_size)));
  max_exp_ru          <= and_vect_bits(result_ru(mantissa_size+exponent_size-1 downto mantissa_size));
  min_mnt_out         <= not(or_vect_bits(result_int(mantissa_size-1 downto 0)));
  min_exp_out         <= not(or_vect_bits(result_int(mantissa_size+exponent_size-1 downto mantissa_size)));
  max_exp_out         <= and_vect_bits(result_int(mantissa_size+exponent_size-1 downto mantissa_size));
  inf_out             <= '1' when ((max_exp_out = '1') and (result_int(mantissa_size-1 downto 0) = (0 to mantissa_size-1 => '0'))) else '0';
  result              <= result_int;
  inexact_round_int   <= inexact_div   when op_mode(3) else inexact_sqrt   when op_mode(4) else inexact_round;
  precision_round_int <= precision_div when op_mode(3) else precision_sqrt when op_mode(4) else precision_round;
  result_ru_int       <= result_div    when op_mode(3) else fp_sqrt_res    when op_mode(4) and not bypass_sqrt else result_ru;

  process(clk_i)  
  begin
    if rising_edge(clk_i) then
      data_a_lat <= data_a;
    end if;  
  end process; 

  MUX_FP_RES : process(all)
  begin
    exp_shift     <= exp_shift_mul;
    exp_int       <= exp_mul_int;
    underflow     <= '0';
    overflow      <= '0';
    add_sub_en    <= '0';
    mul_en        <= '0';
    div_en        <= '0';
    sqrt_en       <= '0';
    max_exp_in    <= '0';
    if valid_i then
      if or_vect_bits(op_mode(1 downto 0)) then -- add or sub
        add_sub_en    <= '1';
        bypass        <= bypass_add;
        result_bypass <= result_bypass_add;
        exp_int       <= "00" & exp_add_int;
        exp_shift     <= exp_shift_add;
        sign_res      <= sign_res_add;
        inexact_norm  <= inexact_add_sub;
        max_exp_in    <= max_exp_a or max_exp_b;
      elsif op_mode(2) then -- mul
        mul_en        <= '1';
        bypass        <= bypass_mul;
        result_bypass <= result_bypass_mul;
        sign_res      <= sign_res_mul;
        underflow     <= underflow_mul;
        overflow      <= overflow_mul;
        inexact_norm  <= inexact_mul;
        max_exp_in    <= max_exp_a or max_exp_b;
      elsif op_mode(3) then -- div
        div_en        <= '1';
        bypass        <= bypass_div;
        result_bypass <= result_bypass_div;
        sign_res      <= sign_res_div;
        overflow      <= overflow_div;
        max_exp_in    <= max_exp_a or max_exp_b;
      elsif op_mode(4) then -- sqrt
        bypass        <= bypass_sqrt;
        result_bypass <= result_bypass_sqrt;
        sign_res      <= sign_res_sqrt;
        max_exp_in    <= max_exp_a;
        sqrt_en       <= '1';
      else -- fma
        bypass        <= bypass_fma;
        result_bypass <= result_bypass_fma;
        sign_res      <= sign_res_fma;
        max_exp_in    <= max_exp_a or max_exp_b or max_exp_c;
      end if;
    end if;
  end process;

  fp_classify_inst : fp_classify
  generic map (
    size          => size,
    exponent_size => exponent_size,
    mantissa_size => mantissa_size
  )
  port map (
    clk_i      => clk_i,
    rst_ni     => rst_ni,
    data_a     => data_a,
    data_b     => data_b,
    data_c     => data_c,
    sign_a     => sign_a,
    sign_b     => sign_b,
    sign_c     => sign_c,
    exp_a      => exp_a,
    exp_b      => exp_b,
    exp_c      => exp_c,
    mnt_a      => mnt_a,
    mnt_b      => mnt_b,
    mnt_c      => mnt_c,
    zero_a     => zero_a,
    zero_b     => zero_b,
    zero_c     => zero_c,
    neg_zero_a => neg_zero_a,
    neg_zero_b => neg_zero_b,
    neg_zero_c => neg_zero_c,
    norm_a     => norm_a,
    norm_b     => norm_b,
    norm_c     => norm_c,
    max_exp_a  => max_exp_a,
    max_exp_b  => max_exp_b,
    max_exp_c  => max_exp_c,
    inf_a      => inf_a,
    inf_b      => inf_b,
    inf_c      => inf_c,
    nan_a      => nan_a,
    nan_b      => nan_b,
    nan_c      => nan_c,
    snan_a     => snan_a,
    snan_b     => snan_b,
    snan_c     => snan_c,
    qnan_a     => qnan_a,
    qnan_b     => qnan_b,
    qnan_c     => qnan_c,
    opp_signs  => opp_signs,
    comp_exp   => comp_exp,
    comp_a_b   => comp_a_b
  );

  fp_add_inst : fp_add
  generic map (
    size          => size,
    exponent_size => exponent_size,
    mantissa_size => mantissa_size,
    bias          => bias
  )
  port map (
    clk_i             => clk_i,
    rst_ni            => rst_ni,
    mode              => mode,
    add_sub_en        => add_sub_en,
    data_a            => data_a,
    data_b            => data_b,
    sign_a            => sign_a,
    sign_b            => sign_b,
    exp_a             => exp_a,
    exp_b             => exp_b,
    mnt_a             => mnt_a,
    mnt_b             => mnt_b,
    zero_a            => zero_a,
    zero_b            => zero_b,
    neg_zero_a        => neg_zero_a,
    neg_zero_b        => neg_zero_b,
    norm_a            => norm_a,
    norm_b            => norm_b,
    inf_a             => inf_a,
    inf_b             => inf_b,
    nan_a             => nan_a,
    nan_b             => nan_b,
    snan_a            => snan_a,
    snan_b            => snan_b,
    opp_signs         => opp_signs,
    comp_exp          => comp_exp,
    comp_a_b          => comp_a_b,
    inexact_add_sub   => inexact_add_sub,
    invalid_add_sub   => invalid_add_sub,
    exp_add_int       => exp_add_int,
    exp_shift_add     => exp_shift_add,
    mnt_res_add       => mnt_res_add,
    sign_res_add      => sign_res_add,
    bypass_add        => bypass_add,
    result_bypass_add => result_bypass_add,
    ready_add         => ready_add
  );

  fp_mul_inst : fp_mul
  generic map (
    size          => size,
    exponent_size => exponent_size,
    mantissa_size => mantissa_size,
    bias          => bias
  )
  port map (
      clk_i             => clk_i,
      rst_ni            => rst_ni,
      mul_en            => mul_en,
      mode              => mode,
      data_a            => data_a,
      data_b            => data_b,
      sign_a            => sign_a,
      sign_b            => sign_b,
      exp_a             => exp_a,
      exp_b             => exp_b,
      mnt_a             => mnt_a,
      mnt_b             => mnt_b,
      zero_a            => zero_a,
      zero_b            => zero_b,
      norm_a            => norm_a,
      norm_b            => norm_b,
      inf_a             => inf_a,
      inf_b             => inf_b,
      nan_a             => nan_a,
      nan_b             => nan_b,
      snan_a            => snan_a,
      snan_b            => snan_b,
      opp_signs         => opp_signs,
      comp_a_b          => comp_a_b,
      precision         => open,
      inexact_mul       => inexact_mul,
      invalid_mul       => invalid_mul,
      exp_mul_int       => exp_mul_int,
      exp_shift_mul     => exp_shift_mul,
      mnt_res_mul       => mnt_res_mul,
      sign_res_mul      => sign_res_mul,
      bypass_mul        => bypass_mul,
      result_bypass_mul => result_bypass_mul,
      underflow_mul     => underflow_mul,
      overflow_mul      => overflow_mul,
      ready_mul         => ready_mul
  );

  fp_div_inst : fp_div
  generic map (
    size          => size,
    exponent_size => exponent_size,
    mantissa_size => mantissa_size,
    bias          => bias
  )
  port map (
    clk_i         => clk_i,
    rst_ni        => rst_ni,
    div_en        => div_en,
    op_mode       => op_mode,
    data_a        => data_a,
    data_b        => data_b,
    sign_a        => sign_a,
    sign_b        => sign_b,
    exp_a         => exp_a,
    exp_b         => exp_b,
    mnt_a         => mnt_a,
    mnt_b         => mnt_b,
    zero_a        => zero_a,
    zero_b        => zero_b,
    neg_zero_a    => neg_zero_a,
    neg_zero_b    => neg_zero_b,
    norm_a        => norm_a,
    norm_b        => norm_b,
    inf_a         => inf_a,
    inf_b         => inf_b,
    nan_a         => nan_a,
    nan_b         => nan_b,
    snan_a        => snan_a,
    snan_b        => snan_b,
    comp_a_b      => comp_a_b,
    result_div    => result_div,
    fp_div_ready  => fp_div_ready,
    precision_div => precision_div,
    inexact_div   => inexact_div,
    overflow_div  => overflow_div,
    div_zero      => div_zero,
    invalid_div   => invalid_div
  );

  fp_sqrt_inst : fp_sqrt
  generic map(
    size          => size,
    exponent_size => exponent_size,
    mantissa_size => mantissa_size,
    bias          => bias
  )  
  port map(
    clk_i              => clk_i,
    rst_ni             => rst_ni,
    sqrt_en            => sqrt_en,
    op_mode            => op_mode,
    data_a             => data_a,
    sign_a             => sign_a,
    exp_a              => exp_a,
    mnt_a              => mnt_a,
    zero_a             => zero_a,
    neg_zero_a         => neg_zero_a,
    norm_a             => norm_a,
    inf_a              => inf_a,
    nan_a              => nan_a,
    snan_a             => snan_a,
    qnan_a             => qnan_a,
    fp_sqrt_ready      => fp_sqrt_ready,
    fp_sqrt_res        => fp_sqrt_res,
    bypass_sqrt        => bypass_sqrt,
    result_bypass_sqrt => result_bypass_sqrt,
    precision_sqrt     => precision_sqrt,
    inexact_sqrt       => inexact_sqrt,
    invalid_sqrt       => invalid_sqrt
  );

  fp_norm_inst : fp_norm
  generic map (
    size          => size,
    exponent_size => exponent_size,
    mantissa_size => mantissa_size,
    bias          => bias
  )
  port map (
    clk_i           => clk_i,
    rst_ni          => rst_ni,
    op_mode         => op_mode,
    round_mode      => round_mode,
    norm_a          => norm_a,
    norm_b          => norm_b,
    inf_a           => inf_a,
    inf_b           => inf_b,
    nan_a           => nan_a,
    nan_b           => nan_b,
    comp_exp        => comp_exp,
    comp_a_b        => comp_a_b,
    inexact_norm    => inexact_norm,
    exp_int         => exp_int,
    exp_shift       => exp_shift,
    mnt_res_add     => mnt_res_add,
    mnt_res_mul     => mnt_res_mul,
    sign_res        => sign_res,
    bypass          => bypass,
    result_bypass   => result_bypass,
    underflow       => underflow,
    uf_3_trig       => uf_3_trig,
    overflow_norm   => overflow_norm,
    precision_round => precision_round,
    inexact_round   => inexact_round,
    result_ru       => result_ru
  );

  fp_round_inst : fp_round
  generic map(
    size          => size,
    exponent_size => exponent_size,
    mantissa_size => mantissa_size
  )
  port map (
    clk_i           => clk_i,
    rst_ni          => rst_ni,
    op_mode         => op_mode,
    round_mode      => round_mode,
    result_ru       => result_ru_int,
    overflow_norm   => overflow_norm,
    inexact_round   => inexact_round_int,
    precision_round => precision_round_int,
    opp_signs       => opp_signs,
    comp_a_b        => comp_a_b,
    result          => result_int
  );

end architecture behavioral;