
********************************************************************************
***REDOING THESIS ON FORECASTING RESIDENTIAL ELECTRICITY DEMAND USING ECM
***Part 1: Data Preparation
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
	
	* Use data only starting 1993
	drop if year <1993
	
* Prepare the time-series variables
tsset year

	* Convert numeric data into log
	gen ln_cons = ln(hhcons)
	gen ln_price = ln(real_price)
	gen ln_hhfe = ln(real_hhfe)
	gen ln_temp = ln(temp)
	
	* Label the variables
	label var ln_cons "Log of HH electricity consumption"
	label var ln_price "Log of real electricity prices (MERALCO)"
	label var ln_hhfe "Log of real HH final consumption expenditure"
	label var ln_temp "Log of annual average temperature"
	
	* Identify order of integration using Dickey-Fuller
	dfuller ln_cons, lags(2) //HH electricity
	dfuller ln_price, lags(0) //Real price
	dfuller ln_hhfe, lags(0) //HH exp
	dfuller ln_temp, lags(0) //Temp
	
	* Test when variables are first-differenced
	dfuller D.ln_cons, lags(0) //HH electricity
	dfuller D.ln_price, lags(0) //Real price
	dfuller D.ln_hhfe, lags(7) //HH exp
	
	* Note: For the Elliott -Rothenberg - Stock Test, refer to R code
	save "${filename}export_to_R.dta", replace
	

* Perform ECM and ARDL

reg D.ln_cons D.ln_hhfe LD.ln_price D.ln_temp L.ln_cons L.ln_hhfe //ECM
reg D.ln_cons D.ln_hhfe LD.ln_price D.ln_temp //ARDL

* Perform ECM using training dataset
reg D.ln_cons D.ln_hhfe LD.ln_price D.ln_temp L.ln_cons L.ln_hhfe if year<2012
predict Dln_cons_hat

* Determine actual predicted values
gen hhcons_pred= hhcons if year<=2011
replace hhcons_pred=exp(L.ln_cons+Dln_cons_hat) if year>2011 & year<=2015

*Forecast error
gen e_a=hhcons-hhcons_pred if year>2011 & year<=2015
*MSE 
gen sq_e_a=e_a^2
*MPE
gen pe_a=e_a/hhcons*100
*MAPE
gen ape_a=abs(pe_a)
tabstat sq_e_a pe_a ape_a

drop Dln_cons_hat - ape_a

* Perform ARDL using training dataset
reg D.ln_cons D.ln_hhfe LD.ln_price D.ln_temp  if year<2012
predict Dln_cons_hat


* Determine actual predicted values
gen hhcons_pred= hhcons if year<=2011
replace hhcons_pred=exp(L.ln_cons+Dln_cons_hat) if year>2011 & year<=2015

*Forecast error
gen e_a=hhcons-hhcons_pred if year>2011 & year<=2015
*MSE 
gen sq_e_a=e_a^2
*MPE
gen pe_a=e_a/hhcons*100
*MAPE
gen ape_a=abs(pe_a)
tabstat sq_e_a pe_a ape_a

