-------------------------------------------------------------------------------
--
--  This file is part of the ImageZero Encoder.
--
--  Description:  
--    Top level of IZ Encoder
--
--  M_pixel: pixel depth in bits(includes the components R, G, B)
--  N_comp: size of pixel component,  N_Comp*3 = M_Pixel
--
-- N_comp_add: number of added bits for pixel preocessing (depends on the predictor used)
-- For predict3avgplane, N_comp_add is 1, this value is always less than 8
--
-- Width: Image Width (in pixels)
-- Height: Image Height (in pixels)
-- T_Predict: Pixel predictor
--
-- To use predict3avgplane -> T_Predict=0;
-- To use predict3alpha -> T_Predict=1;
-- To use predict3avg -> T_Predict=2;
-- To use predict3med -> T_Predict=3;
-- To use predict3plane -> T_Predict=4;
--
--  Author(s): Martín Vázquez, martin.o.vazquez@gmail.com
-- Date: 12/12/2018
-- 
-------------------------------------------------------------------------------
--
--  Copyright (c) 2018 LabSET
--
--  This source file may be used and distributed without restriction provided
--  that this copyright statement is not removed from the file and that any
--  derivative work contains the original copyright notice and the associated
--  disclaimer.
--
--  This source file is free software: you can redistribute it and/or modify it
--  under the terms of the GNU Lesser General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  This source file is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU Lesser General Public License
--  along with the ImageZero Encoder.  If not, see http://www.gnu.org/licenses
--
------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.numeric_std.ALL;
use IEEE.math_real.all;
use work.my_package.all;

entity encoder_IZ is
    generic (M_pixel: integer:=32; N_comp:integer:=8; N_comp_add:integer:=1;
             height:integer:=1024; Width:integer:=1024; T_predict: integer:=0);
    port ( clk, rst : in std_logic;
           v_pix: in std_logic_vector(M_pixel-1 downto 0);
           valid: in std_logic;
           
           o_valid: out std_logic;
           o_data: out std_logic_vector (31 downto 0);
           o_last: out std_logic);
     end encoder_IZ;


architecture Behavioral of encoder_IZ is

    component ControlUnit
        generic ( height:integer:=5; Width:integer:=5);
        Port ( clk, rst : in std_logic;
               start: in std_logic;
               valid: in std_logic;
               rst_cx: out std_logic;
             
               done: out std_logic; --integer(ceil(log2(real(
               rdy: out std_logic;
               cnt_x: out std_logic_vector (log2sup(height)-1 downto 0);
               --cnt_x: out std_logic_vector (log2sup(height)-1 downto 0);
               cnt_y: out std_logic_vector (log2sup(Width)-1 downto 0)
               );
    end component;
   
    component reg
        generic (N: integer:= 8);
        Port ( clk, rst, en : in std_logic;
               i: in std_logic_vector(N-1 downto 0);
               q : out std_logic_vector (N-1 downto 0));
    end component;
   
    component Predict
        generic (N: integer:= 8; T_predict:integer:=0);
        port ( mode_predict : in std_logic_vector (1 downto 0);
               x, y, xy : in std_logic_vector (N-1 downto 0);
               p : out std_logic_vector (N-1 downto 0));
    end component;
   
    component PriorityEncoder
        generic (N:integer:=9);
        port ( i : in std_logic_vector(N-1 downto 0);
               o : out std_logic_vector (3 downto 0));
    end component;
   
    component Table_Dbits
        Port ( i : in std_logic_vector (7 downto 0);
               o : out std_logic_vector (7 downto 0));
    end component;

    component Table_Dcount
        Port ( i : in std_logic_vector (7 downto 0);
               o : out std_logic_vector (3 downto 0));
    end component;

    signal start:  std_logic;

    -- to set N_comp_add leading 0's
    signal  zeros: std_logic_vector(N_comp_add-1 downto 0);
     
    -- control signals
    signal en_regs: std_logic;
    signal rst_cx: std_logic;

    -- signals to determine the pixel position to process
    signal cnt_x: std_Logic_vector(Log2sup(height)-1 downto 0);
    signal cnt_y: std_Logic_vector(Log2sup(width)-1 downto 0);
    signal data_rdy: std_logic;  

 

    -- predictor type
    signal mode_predict: std_logic_vector(1 downto 0);

    -- corresponding to predictors
    signal predict_r, predict_g, predict_b: std_logic_vector(N_comp+N_comp_add-1 downto 0);

    -- pixeles con procesamiento de predictores
    signal pix_sub_r, pix_sub_g, pix_sub_b: std_logic_vector(N_comp+N_comp_add-1 downto 0);

    -- forward transform pixels
    signal pix_fwt_r, pix_fwt_g, pix_fwt_b: std_logic_vector(N_comp+N_comp_add-1 downto 0);

    --  unsigned pixels
    signal pix_us_r, pix_us_g, pix_us_b: std_logic_vector(N_comp+N_comp_add-1 downto 0);
   
    -- nls in algorithm  
    signal nl, nl_r, nl_g, nl_b, nl_rg, nl_pipe:std_logic_vector(3 downto 0); -- va de 0 a 15,
       
    signal next_cx, reg_cx: std_logic_vector(7 downto 0);

    signal dbits, dbits_pipe: std_logic_vector(7 downto 0);
    signal dcount: std_logic_vector(3 downto 0);
   
   
    type Tline_pixels is array(0 to Width+2) of std_logic_vector(N_comp+N_comp_add-1 downto 0);
    signal pixels_r, pixels_g, pixels_b: Tline_pixels;
   
    signal o_pixel, o_pixel_pipe: std_logic_vector(31 downto 0);
      signal o_reg: std_logic_vector(31 downto 0);
    --signal o_count: std_logic_vector(5 downto 0);
    signal m, m_pipe: std_logic_vector(4 downto 0); --Nueva
    signal o_reg_buf_count: natural;
   
    signal db_pipe: std_logic_vector(7 downto 0);
signal dcount_pipe:  std_logic_vector(3 downto 0);
--signal nl_pipe:  std_logic_vector(3 downto 0);
signal r_pipe, g_pipe, b_pipe : std_logic_vector (N_comp-1 downto 0);
signal valid_pipe, done_pipe: std_logic; 
signal next_o_reg_buf, o_reg_buf: std_logic_vector (63 downto 0);
signal res_tmp: std_logic_vector(95 downto 0);
 signal next_index, index: std_logic_vector (5 downto 0);
 signal next_data_tx, data_tx: std_logic;
 signal done: std_logic;

   
   

begin

uCtrl: ControlUnit generic map (height => height, Width => Width)
                       port map (clk => clk, rst => rst, start => start, valid => valid,  rdy => data_rdy,
                                   rst_cx => rst_cx,  done => done,
                                   cnt_x => cnt_x, cnt_y => cnt_y);
   
    -- ========================
    -- FIFO generation of latests Width+1 pixels and the current
    -- ============
   
   
    zeros <= (others => '0');
   
    pixels_r(0) <= (zeros&v_pix(M_pixel-1 downto 2*N_comp));
    pixels_g(0) <= (zeros&v_pix(2*N_comp-1 downto N_comp));
    pixels_b(0) <= (zeros&v_pix(N_comp-1 downto 0));
   
    en_regs <= '1' when (valid = '1' and v_pix /=x"FFFFFFFF") else '0';
    start  <= '1' when (valid = '1' and v_pix =x"FFFFFFFF") else '0';  
   
    GenLinePixels: for I in 0 to Width+1 generate  -- va desde 0 a Width+1

        eregRed: reg generic map (N => N_comp+N_comp_add)
                     port map ( clk => clk, rst => rst,
                          en => en_regs, i => pixels_r(I), q =>pixels_r(I+1));
       
        eregGreen: reg generic map (N => N_comp+N_comp_add)
                       port map ( clk => clk, rst => rst,
                        en => en_regs, i => pixels_g(I), q =>pixels_g(I+1));
             
        eregBlue: reg generic map (N => N_comp+N_comp_add)
                     port map ( clk => clk, rst => rst,
                     en => en_regs, i => pixels_b(I), q =>pixels_b(I+1));
                                                             
    end generate;
   
    -- ========================
    -- Prediction
    -- ========================
        -- position analysis          
        mode_predict <= "00" when (cnt_x=0 and cnt_y=0) else
                        "01" when (cnt_x=0) else
                        "10" when (cnt_y=0) else
                        "11"; -- caso cnt_x /= 0 y cnt_y /= 0      


    -- component predictions
        ePredictR: Predict generic map (N => N_comp+N_comp_add, T_predict => T_predict)
                    port map (mode_predict => mode_predict,
                            x =>  pixels_r(2), y => pixels_r(Width+1),
                            xy => pixels_r(Width+2), p => predict_r);
               
        ePredictG: Predict generic map (N => N_comp+N_comp_add)
                    port map (mode_predict => mode_predict,
                            x =>  pixels_g(2), y => pixels_g(Width+1),
                            xy => pixels_g(Width+2), p => predict_g);
                               
        ePredictB: Predict generic map (N => N_comp+N_comp_add)
                    port map (mode_predict => mode_predict,
                            x =>  pixels_b(2), y => pixels_b(Width+1),
                            xy => pixels_b(Width+2), p => predict_b);
           
   
   
   
    pix_sub_r <= pixels_r(1) - predict_r;
    pix_sub_g <= pixels_g(1) - predict_g;
    pix_sub_b <= pixels_b(1) - predict_b;

-- forward transform
    pix_fwt_r <= pix_sub_r;
    pix_fwt_g <= pix_sub_g - pix_sub_r;
    pix_fwt_b <= pix_sub_b - pix_sub_r;
   
    -- to unsigned
    pix_us_r <= (pix_fwt_r(N_comp+N_comp_add-2 downto 0)&'0') when (pix_fwt_r(N_comp+N_comp_add-1)='0') else
                --((not (pix_fwt_r(N_comp-2 downto 0))&'0')+1);
                (not (pix_fwt_r(N_comp+N_comp_add-2 downto 0))&'1');
   
    pix_us_g <= (pix_fwt_g(N_comp+N_comp_add-2 downto 0)&'0') when (pix_fwt_g(N_comp+N_comp_add-1)='0') else
                (not (pix_fwt_g(N_comp+N_comp_add-2 downto 0))&'1');
                   
    pix_us_b <= (pix_fwt_b(N_comp+N_comp_add-2 downto 0)&'0') when (pix_fwt_b(N_comp+N_comp_add-1)='0') else
                (not (pix_fwt_b(N_comp+N_comp_add-2 downto 0))&'1');

                                                       
    -- nl calculation
    EEncR: PriorityEncoder generic map (N => N_comp+N_comp_add)
                            port map ( i => pix_us_r, o => nl_r);
       
    EEncG: PriorityEncoder generic map (N => N_comp+N_comp_add)
                           port map ( i => pix_us_g, o => nl_g);
                   
    EEncB: PriorityEncoder generic map (N => N_comp+N_comp_add)
                           port map ( i => pix_us_b, o => nl_b);
                           
    nl_rg <=    nl_r when (nl_r>nl_g) else
                nl_g;
               
    nl <=   nl_b when (nl_b>nl_rg) else
            nl_rg;                                  
   
    -- cx calculation
    next_cx <= x"77" when (rst_cx='1') else
                (reg_cx(3 downto 0)&nl);
               
    Eregister_cx: reg generic map (N => 8)
                        port map (clk => clk, rst=> rst,  
                                    en => en_regs, i => next_cx, q => reg_cx);            


   
     Etable_db: Table_Dbits port map (i => next_cx, o => dbits);            

     Etable_dcount: Table_Dcount port map (i => next_cx, o => dcount);  
     
m <= (nl &'0')+nl +dcount;   
o_pixel((3*conv_integer(nl)+conv_integer(dcount))-1 downto 0) <= dbits(conv_integer(dcount) downto 0)& pix_us_g(conv_integer(nl) downto 0) & pix_us_r(conv_integer(nl) downto 0) & pix_us_b(conv_integer(nl) downto 0);            

pipe: process(clk, rst)
begin
    if rst = '1' then 
o_pixel_pipe <= (others => '0');
        m_pipe <= (others => '0');
        valid_pipe <= '0'; 
        done_pipe <= '0';
        nl_pipe <= (others => '0'); 
    elsif (rising_edge(clk)) then 
    
        o_pixel_pipe <= o_pixel;
        m_pipe <= m; 
        valid_pipe <= valid; 
        done_pipe <= done;
        nl_pipe <= nl; 
    end if; 
end process;  


--m <= (nl_pipe &'0')+nl_pipe +dcount_pipe;
--o_pixel((3*conv_integer(nl_pipe)+conv_integer(dcount_pipe))-1 downto 0) <= dbits_pipe(conv_integer(dcount_pipe) downto 0)& g_pipe(conv_integer(nl_pipe) downto 0) & r_pipe(conv_integer(nl_pipe) downto 0) & b_pipe(conv_integer(nl_pipe) downto 0);  
     
   
--process (o_reg_buf,m_pipe, o_pixel_pipe)
--begin
--    if done_pipe= '1' then
--       res_tmp <= (others => '0');
--       res_tmp(conv_integer(m_pipe)-1 downto 0) <= o_pixel_pipe(conv_integer(m_pipe)-1 downto 0);--**conv_integer(m);

--    else
--        res_tmp <= o_reg_buf(63-conv_integer(m_pipe) downto 0) & o_pixel_pipe(conv_integer(m_pipe)-1 downto 0);
--    end if;
--end process;
   
--res_tmp <= o_reg_buf(63-conv_integer(m_pipe) downto 0) & o_pixel(conv_integer(m_pipe)-1 downto 0);

   
               
  ---  next_o_reg_buf <= res_tmp(63 downto 0);
   
    next_index <= "000000" when (done = '1') else
                  (m_pipe + index) when (index < x"32")  else
                  ((m_pipe+ index) and "100000");
   
    next_data_tx <= '1' when ((index > x"32")or (done_pipe='1')) else '0';  
 
   
    gen_ount: process(rst, clk)
    begin
        if rst = '1' then
            o_reg_buf <= (others => '0');
            index <=  (others => '0');
            data_tx <= '0';
        elsif rising_edge(clk) then
            if valid_pipe = '1' then
                case (m_pipe) is 
                 when "00001" =>  o_reg_buf <=  o_reg_buf(62 downto 0) & o_pixel(0 );
                 when "00010" =>  o_reg_buf <=  o_reg_buf(61 downto 0) & o_pixel(1 downto 0);
                 when "00011" =>  o_reg_buf <=  o_reg_buf(60 downto 0) & o_pixel(2 downto 0);
                 when "00100" =>  o_reg_buf <=  o_reg_buf(59 downto 0) & o_pixel(3 downto 0);
                when "00101" =>  o_reg_buf <=  o_reg_buf(58 downto 0) & o_pixel(4 downto 0);
                when "00110" =>  o_reg_buf <=  o_reg_buf(57 downto 0) & o_pixel(5 downto 0);
                when "00111" =>  o_reg_buf <=  o_reg_buf(56 downto 0) & o_pixel(6 downto 0);
                when "01000" =>  o_reg_buf <=  o_reg_buf(55 downto 0) & o_pixel(7 downto 0);
                when "01001" =>  o_reg_buf <=  o_reg_buf(54 downto 0) & o_pixel(8 downto 0);
                when "01010" =>  o_reg_buf <=  o_reg_buf(53 downto 0) & o_pixel(9 downto 0);
                when "01011" =>  o_reg_buf <=  o_reg_buf(52 downto 0) & o_pixel(10 downto 0);
                when "01100" =>  o_reg_buf <=  o_reg_buf(51 downto 0) & o_pixel(11 downto 0);
                when "01101" =>  o_reg_buf <=  o_reg_buf(50 downto 0) & o_pixel(12 downto 0);
                when "01110" =>  o_reg_buf <=  o_reg_buf(49 downto 0) & o_pixel(13 downto 0);
                when "01111" =>  o_reg_buf <=  o_reg_buf(48 downto 0) & o_pixel(14 downto 0);
                when "10000" =>  o_reg_buf <=  o_reg_buf(47 downto 0) & o_pixel(15 downto 0);
                when "10001" =>  o_reg_buf <=  o_reg_buf(46 downto 0) & o_pixel(16 downto 0);
                when "10010" =>  o_reg_buf <=  o_reg_buf(45 downto 0) & o_pixel(17 downto 0);
                when "10011" =>  o_reg_buf <=  o_reg_buf(44 downto 0) & o_pixel(18 downto 0);
                when "10100" =>  o_reg_buf <=  o_reg_buf(43 downto 0) & o_pixel(19 downto 0);
                when "10101" =>  o_reg_buf <=  o_reg_buf(42 downto 0) & o_pixel(20 downto 0);
                when "10110" =>  o_reg_buf <=  o_reg_buf(41 downto 0) & o_pixel(21 downto 0);
                when "10111" =>  o_reg_buf <=  o_reg_buf(40 downto 0) & o_pixel(22 downto 0);
                when "11000" =>  o_reg_buf <=  o_reg_buf(39 downto 0) & o_pixel(23 downto 0);
                when "11001" =>  o_reg_buf <=  o_reg_buf(38 downto 0) & o_pixel(24 downto 0);
                when "11010" =>  o_reg_buf <=  o_reg_buf(37 downto 0) & o_pixel(25 downto 0);
                when "11011" =>  o_reg_buf <=  o_reg_buf(36 downto 0) & o_pixel(26 downto 0);
                when "11100" =>  o_reg_buf <=  o_reg_buf(35 downto 0) & o_pixel(27 downto 0);
                when "11101" =>  o_reg_buf <=  o_reg_buf(34 downto 0) & o_pixel(28 downto 0);
                when "11110" =>  o_reg_buf <=  o_reg_buf(33 downto 0) & o_pixel(29 downto 0);
                when "11111" =>  o_reg_buf <=  o_reg_buf(32 downto 0) & o_pixel(30 downto 0);
                when others => o_reg_buf <=  o_reg_buf; 
                
   
                
                end case; 
               -- o_reg_buf <=  o_reg_buf(63-conv_integer(m_pipe) downto 0) & o_pixel(conv_integer(m_pipe)-1 downto 0);--next_o_reg_buf;
                data_tx <= next_data_tx;
                index <= next_index;
           end if;
        end if;
    end process;
   
    o_data <= o_reg_buf(conv_integer(index)+31 downto conv_integer(index));
    o_valid <= data_tx;
    o_last<= done;
       
end Behavioral;
