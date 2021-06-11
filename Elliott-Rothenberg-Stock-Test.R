#####################################################################
###Elliott, Rothenberg, Stock Test
#By: ANGELO SANTOS
#Date: May 29, 2021
#####################################################################
##Load packages
library(haven)
library(tidyverse)
library(urca)

##Set working directory and load data
setwd("/Users/angelogabriellesantos/OneDrive - George Mason University/Forecasting Residential Electricity Demand/Data")
HHElec <- read_dta("export_to_R.dta")

#####################################################################
##Perform ERS test
#####################################################################

#<HHFE>
      #Perform on HHFE
      ers_hhfe <- ur.ers(HHElec$ln_hhfe, type = "P-test", 
          model = "trend", lag.max = 4)
      summary(ers_hhfe)
      
      #Perform on first diff HHFE
      Dln_hhfe <- diff(HHElec$ln_hhfe)
      ers_hhfe <- ur.ers(Dln_hhfe, type = c("P-test"), 
                           model = c("trend"), lag.max = 4)
      summary(ers_hhfe)

#</HHFE>

      
#<ELECTRICITY>
      #Perform on HH electricity consumption
      ers_cons <- ur.ers(HHElec$ln_cons, type = "P-test", 
               model = "trend", lag.max = 4)
      summary(ers_cons)

      #Perform on first diff HH electricity consumption
      Dln_cons <- diff(HHElec$ln_cons)
      ers_Dcons <- ur.ers(Dln_cons, type = "P-test", 
                model = "trend", lag.max = 4)
      summary(ers_Dcons)
      
#</ELECTRICITY>
      

#<PRICES>
      #Perform on HH electricity prices
      ers_price <- ur.ers(HHElec$ln_price, type = "P-test", 
                         model = "trend", lag.max = 2)
      summary(ers_price)
      
      #Perform on first diff HH electricity prices
      Dln_price <- diff(HHElec$ln_price)
      ers_Dprice <- ur.ers(Dln_price, type = "P-test", 
                          model = "trend", lag.max = 2)
      summary(ers_Dprice)
#</PRICES>
      
#<TEMPERATURE>
      #Perform on HH electricity prices
      ers_temp <- ur.ers(HHElec$ln_temp, type = "P-test", 
                          model = "trend", lag.max = 4)
      summary(ers_temp)
      
      #Perform on first diff HH electricity prices
      Dln_temp <- diff(HHElec$ln_temp)
      ers_Dtemp <- ur.ers(Dln_temp, type = "P-test", 
                           model = "trend", lag.max = 4)
      summary(ers_Dtemp)
#</TEMPERATURE>
    
  
      
      

