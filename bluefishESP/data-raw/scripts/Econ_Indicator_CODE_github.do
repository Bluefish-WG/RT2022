*Code created 03.11.2022

/***Steps***
*1. Edit user Macros (add your credencials for accessing ORACLE databases)
2. Edit your fodler directory set up 
3. Edit which years you would like using the LOCAL code! 
4. Ready to run full code file */ 


**STEP 1************************************************ORACLE MACROS******************************************
*******************************if below doesnt work, use this and then run the code for macros odbc load, dsn("sole") 

*test to make sure you can access the databases
clear all
odbc load, dsn ("sole") user("user_name_here") password("password_here") exec( "select  * from cfspp;")
#delimit cr

global myuid "user_name_here"
global mypwd "password_here"

global mysole_conn "dsn(sole) user($myuid) password($mypwd)"
global mynova_conn "dsn(nova) user($myuid) password($mypwd)"

clear
odbc load, exec("select * from cfspp;") $mysole_conn

**Step 2 *****************FOLDER SET UP MACROS*******************************************************

/* Set up global macros to point to folders */

global my_projdir "C:/Users/samantha.L.werner/Desktop/Bluefish_WG/Indicator_Code"
display "$my_projdir"


/******
*****Test****

display "$my_projdir";
display $user;
display $my_projdir;

display ${my_projdir};

*/

**************Step 3************************************
clear all 

***replace your starting and endyear year where the YYYY s are 
global startyear 2000 
global endyear 2021
local startyear 2000 
local endyear 2021

**Step 4. ready to run all code!

************************************************************************************************************************
*******************************************Bluefish Landings*********************************************
************************************************************************************************************************
clear all  

 forvalues yr= `startyear'/`endyear' {
    tempfile new12
    local land `"`land'"`new12'" "'  
    clear
	odbc load,  exec ("SELECT  sum(SPPLNDLB) as total_bluefish from CFDBS.CFDERS`yr' where NESPP3 in '023' ;") $mysole_conn
    gen dbyear=`yr'
	quietly save `new12', emptyok
}
dsconcat `land'


*******create content and variable headers which are consistent with other indicator files 
gen CATEGORY = "Commercial"
gen INDICATOR_NAME = "Commercial_Bluefish_Landings_LBS"
gen SIGN = "N/A"
gen INDICATOR_TYPE = "Socioeconomic" 

rename db YEAR 
rename TOTAL_BLUEFISH DATA_VALUE

***save locally to append later 

*tempfile temp
*save `temp'
*clear all

save "$my_projdir/landings"
************************************************************************************************************************
***********************************Bluefish Vessels*******************************************
**********************************************************************************************************
clear all
 forvalues yr= $startyear/$endyear {
    tempfile new15
    local VES `"`VES'"`new15'" "'  
    clear
	odbc load,  exec ("SELECT  count(unique PERMIT) as N_VESSELS from CFDBS.CFDERS`yr' where NESPP3 in '023' ;") $mysole_conn
    gen dbyear=`yr'
	quietly save `new15', emptyok
}
dsconcat `VES'


****create content and variable headers which are consistent with other indicator files 
gen CATEGORY = "Commercial"
gen INDICATOR_NAME = "N_Commercial_Vessels_Landing_Bluefish"
gen SIGN = "N/A"
gen INDICATOR_TYPE = "Socioeconomic" 

rename db YEAR 
rename N_VESSELS DATA_VALUE


*append using `temp'
*tempfile temp2
*save `temp2'

save "$my_projdir/vessels"

clear all

*******************************************************************************************************
********************Price deflator file **********************************************************************
*************************************************************************************************************

**replace your starting and endyear year where the YYYY s are 
 
import fred GDPDEF, daterange( $startyear $endyear) aggregate(annual,avg) clear
split datest, p(-)
drop datestr3 datestr2
rename datestr1 dbyear
destring dbyear, replace
keep GDPDEF dbyear
save "$my_projdir/GDPDEF_PRICE_DEFLATOR",replace 

clear all


*****************************************************************************************************************
********************Average Annual Ex-vessel prices*************************************************************
*****************************************************************************************************************

 forvalues yr= `startyear'/`endyear' {
    tempfile new16
    local PRICE `"`PRICE'"`new16'" "'  
    clear
	odbc load,  exec ("select (SPPVALUE/SPPLNDLB) as BLUEFISH_PRICE_dol_lb from CFDBS.CFDERS`yr' where NESPP3 in '023' ;") $mysole_conn
    gen dbyear=`yr'
	quietly save `new16', emptyok
}
dsconcat `PRICE'

**get the average price for each year for each year 

sort dbyear
by dbyear: egen AVG_BLUEFISH_PRICE_dol_lb = mean(BLUEFISH_PRICE_DOL_LB)

***keep one record for year
sort dbyear
quietly by dbyear: gen dup = cond(_N==1, 0,_n)
drop if dup > 1

***drop irrelevant information 
keep AVG_BLUEFISH_PRICE_dol_lb dbyear


******************adjust prices to most recent year  dollars ***https://fred.stlouisfed.org/series/USAGDPDEFQISMEI***************************************************


merge 1:1 dbyear using "$my_projdir\GDPDEF_PRICE_DEFLATOR.dta"
drop _merge

gen base_index = GDPDEF if `endyear' == dbyear
**retain base_index for entire column
egen BASE_INDEX = max(base_index)
drop base_index

**create real dollars vs nominal

gen AVG_BLUEFISH_PRICE_REALdol_lb = (AVG_BLUEFISH_PRICE_dol_lb/GDPDEF) * BASE_INDEX

keep AVG_BLUEFISH_PRICE_REALdol_lb dbyear
*********************************
gen CATEGORY = "Commercial"
gen INDICATOR_NAME = "AVG_BLUEFISH_PRICE_REAL_DOLLARS/lb"
gen SIGN = "N/A"
gen INDICATOR_TYPE = "Socioeconomic" 

rename db YEAR 
rename AVG_BLUEFISH_PRICE_REALdol_lb  DATA_VALUE

save "$my_projdir/price"

clear all

*****************************************************************************************************************
********************Total Annual Revenues *************************************************************
*****************************************************************************************************************
 forvalues yr= $startyear/ $endyear {
    tempfile new13
    local rev `"`rev'"`new13'" "'  
    clear
	odbc load,  exec ("select sum(SPPVALUE) as TOTAL_ANNUAL_BLUEFISH_REVENUE from CFDBS.CFDERS`yr' where NESPP3 in '023' ;") $mysole_conn
    gen dbyear=`yr'
	quietly save `new13', emptyok
}
dsconcat `rev'

save "$my_projdir/revs"

clear all
******************adjust prices to most recent year  dollars ***https://fred.stlouisfed.org/series/USAGDPDEFQISMEI***************************************************
use "$my_projdir/revs"
merge 1:1 dbyear using "$my_projdir\GDPDEF_PRICE_DEFLATOR.dta"
drop _merge


local startyear 2000 
local endyear 2021
gen base_index = GDPDEF if `endyear' == dbyear
**retain base_index for entire column
egen BASE_INDEX = max(base_index)
drop base_index

**create real dollars vs nominal

gen TOTAL_BLUEFISH_REVENUE_REALDOL = (TOTAL_ANNUAL_BLUEFISH_REVENUE/GDPDEF) * BASE_INDEX

keep TOTAL_BLUEFISH_REVENUE_REALDO dbyear
*********************************
gen CATEGORY = "Commercial"
gen INDICATOR_NAME = "TOTAL_ANNUAL_BLUEFISH_REVENUE_REAL_DOLLARS"
gen SIGN = "N/A"
gen INDICATOR_TYPE = "Socioeconomic" 

rename db YEAR 
rename TOTAL  DATA_VALUE
******
save "$my_projdir/revs", replace 

*********************************************************************************************************
***********************************fuel prices**********************************************************
********************************************************************************************************
clear all
import fred DDFUELNYH, daterange( $startyear $endyear) aggregate(annual,avg) clear
**Ultra-Low-Sulfur No. 2 Diesel Fuel Prices: New York Harbor


 split datestr, p(-)
 rename datestr1 dbyear
 drop datestr2 datestr3
destring dbyear, replace
rename DDFUELNYH AVERAGE_ANNUAL_DIESEL_FUEL_PRICE

*****************adjust prices to most recent year  dollars ***https://fred.stlouisfed.org/series/USAGDPDEFQISMEI***************************************************


merge 1:1 dbyear using "$my_projdir\GDPDEF_PRICE_DEFLATOR.dta"
drop if _merge==2
drop _merge



local startyear 2000 
local endyear 2021
gen base_index = GDPDEF if `endyear' == dbyear
**retain base_index for entire column
egen BASE_INDEX = max(base_index)
drop base_index

**create real dollars vs nominal

gen AVG_ANNUAL_DIESEL_FUEL_PRICEREAL = (AVERAGE_ANNUAL_DIESEL_FUEL_PRICE/GDPDEF) * BASE_INDEX

 keep AVERAGE_ANNUAL_DIESEL_FUEL_PRICE dbyear
*********************************
gen CATEGORY = "Commercial"
gen INDICATOR_NAME = "AVG_ANNUAL_DIESEL_FUEL_PRICE_REAL_DOLLARS"
gen SIGN = "N/A"
gen INDICATOR_TYPE = "Socioeconomic" 

rename db YEAR 
rename AVERAGE_ANNUAL_DIESEL_FUEL_PRICE  DATA_VALUE

save "$my_projdir/fuel"

*append using `temp4'
****************save final sheet

clear all  
use "$my_projdir/landings"
append using "$my_projdir/vessels"
append using "$my_projdir/price"
append using "$my_projdir/revs"
append using "$my_projdir/fuel"



save "$my_projdir/SOCIEOECONOMIC_COMMERCIAL_INDICATORS_FINAL" 

**export to excel 
 export excel using "$my_projdir\SOCIEOECONOMIC_COMMERCIAL_INDICATORS_FINAL.xls", replace firstrow(variable)


 *end result is 5 seperate data files for ex-vessel price, landings, number of vessels, revenues, and fuel as well as a single excel file with all of the indicators with porper varaible name formatting. 
 
 *******************END CODE*******************
 
