
********************************************************************************
***REDOING THESIS ON FORECASTING RESIDENTIAL ELECTRICITY DEMAND USING ECM
***Part 2: Create charts
*By: Angelo Santos
*Date: 05-29-2021
********************************************************************************

* Prepare working directory
global filename "/Users/angelogabriellesantos/OneDrive - George Mason University/Forecasting Residential Electricity Demand/Data/"

* Load dataset and perform tidying tasks
use "${filename}thesis_residential.dta", clear

	* Label the variabes
	label var year "Year data was reported"
	label var hhcons "Annual residential electricity consumption for entire Philippines"
	label var price "Nominal price for residential customers of MERALCO"
	label var real_price "Price for residential customers of MERALCO (GDP-deflated)"
	label var gdp "Nominal GDP"
	label var real_gdp "GDP in constant prices"
	label var temp "Annual average temperature recorded by Univ of East Anglia"
	label var hhfe "Nominal Household final consumption expenditure"
	label var real_hhfe "Household final consumption expenditure in constant prices"

	* Drop other variables not needed
	drop urb ngprice real_ngprice usngprice real_usngprice population gdp_def R S
	
	
* Line graph for HH elec consumption
gen hhcons_lab = hhcons/1000

line hhcons_lab year if year<=2015 & year>=1982, ytitle("Residential electricity consumption ('000 GwH)") ///
	xtitle("Year") graphregion(color(white)) xlabel(1980(5)2015) 

* Line graph for Fig 4.1
line hhcons_lab year if year<=2015 & year>=1993, ytitle("Residential electricity consumption ('000 GwH)") ///
	xtitle("Year") graphregion(color(white)) xlabel(1990(5)2015) 

* Electricity prices (constant)
line real_price year if year<=2015 & year>=1993, ytitle("MERALCO residential electricity prices (2000 = 100)") ///
	xtitle("Year") graphregion(color(white)) xlabel(1990(5)2015) 
	
** HHFE (constant)
gen hhfe_lab = real_hhfe/1000
line hhfe_lab year if year<=2015 & year>=1993, ytitle("Household consumption expenditures, Billions PhP (2000 = 100)", size(small)) ///
	xtitle("Year") graphregion(color(white)) xlabel(1990(5)2015)

** Temperature
line temp year if year<=2015 & year>=1993, ytitle("Annual average temperature") ///
	xtitle("Year") graphregion(color(white)) xlabel(1990(5)2015)


