-------------------------------------------------------------------------
--	Package File my_package
--
--	Purpose: This package defines supplemental types, subtypes, 
--		 constants, and functions 
-------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

package my_package is
	
--	constant NDigit: integer := 16;-- 140
--	constant MDigit: integer := 16;-- 140
	constant NBit: integer := 54;--468;
	
	attribute RLOC: string; 	
	function itoa(int: integer range 0 to 999) return string;
	function log2sup(num: natural) return natural;

end my_package;


package body my_package is


	function log2sup (num: natural) return natural is
		variable i,pw: natural;
	begin
		i := 0; pw := 1;
    while(pw < num) loop
      i := i+1; pw := pw*2;
		end loop;
		return i;
	end log2sup;

	function itoa (int: integer range 0 to 999) return string is
		constant nDigits : integer := 3;

		type look_up is array (0 to 9) of character;
		constant convTable : look_up := ('0','1','2','3','4','5','6','7','8','9');

		variable actValue, compValue, unitPow : integer := 0;
		variable strInt : string(1 to nDigits) := "000";
	begin
		actValue := int;

		for ePow in 1 to nDigits loop
			
			unitPow := 10**(nDigits-ePow);  					
  	
			for unitRange in 1 to 10 loop
				compValue := unitPow * unitRange;
				if compValue > actValue then
					actValue := actValue - unitPow*(unitRange-1);
					strInt(ePow) := convTable(unitRange-1);
					exit;
				end if;
			end loop;
		end loop;

		return strInt;
	end itoa;

end my_package;

