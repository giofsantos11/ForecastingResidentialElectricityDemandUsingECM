#####################################################################
###Performing the ARDL for full data and within sample simulations
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

##Add variables in first diff
    Dln_hhfe <- diff(HHElec$ln_hhfe)
    Dln_cons <- diff(HHElec$ln_cons)
    Dln_price <- diff(HHElec$ln_price)
    Dln_temp <- diff(HHElec$ln_temp)

##Create lagged variables
    ##Price
    LDln_price = lag(Dln_price)
    
    ##ln_cons
    Lln_cons = na.omit(lag(HHElec$ln_cons))
    
    ##ln_hhfe
    Lln_hhfe = na.omit(lag(HHElec$ln_hhfe))
    
##ARDL
model.ardl <- lm(Dln_cons ~ Dln_hhfe+ LDln_price + Dln_temp)
summary(model.ardl)

##Predict point estimates and intervals
yhat.ardl <- predict(model.ardl, type = "response")
pred.ardl <- predict(model.ardl, interval = "confidence")

##Create dataframe for actual and predict HH cons
df.cons <- data.frame(Dln_cons[2:22], yhat.ardl, HHElec$year[3:23], 
              Dln_hhfe[2:22], LDln_price[2:22], Dln_temp[2:22], Lln_cons[2:22],
              Lln_hhfe[2:22], pred.ardl)

##Rename the variables in the created data frame
colnames(df.cons) <- c("Dln_cons", "yhat.ardl", "year", "Dln_hhfe", "LDln_price",
                      "Dln_temp", "Lln_cons", "Lln_hhfe", "fit", "lower", "upper")

##Merge complete years dataset
df.full <- right_join(df.cons, HHElec, by="year")
df.full <- df.full[order(df.full$year),]

##Transform point estimate in levels
df.full$ln.cons.sim<- df.full$yhat.ardl + df.full$Lln_cons
df.full$cons.sim <- exp(df.full$ln.cons.sim)

##Transform upper and lower bound estimates in levels
#df.full$up.yhat.ecm <- df.full$yhat.ecm + df.full$upper
#df.full$low.yhat.ecm <- df.full$yhat.ecm - df.full$lower

    #Start with upper
    df.full$up.ln.cons.sim<- df.full$upper + df.full$Lln_cons
    df.full$up.cons.sim <- exp(df.full$up.ln.cons.sim)

    #Lower
    df.full$low.ln.cons.sim<- df.full$lower + df.full$Lln_cons
    df.full$low.cons.sim <- exp(df.full$low.ln.cons.sim)
    

##Prepare line graph comparing actual and predicted with confidence interval
source("nogrid.R")
df.full$hhcons.lab = df.full$hhcons/1000
df.full$cons.sim.lab = df.full$cons.sim/1000
df.full$low.cons.sim.lab = df.full$low.cons.sim/1000
df.full$up.cons.sim.lab = df.full$up.cons.sim/1000

##Create line graph (within sample)
ggplot(data = df.full)+geom_line(aes(x = year, y = cons.sim.lab), color = "#ef8a62", 
  linetype = "dashed") +geom_line(aes(x = year, y = hhcons.lab), color="#67a9cf")+
  geom_ribbon(aes(ymin=low.cons.sim.lab,ymax=up.cons.sim.lab, x = year),
  alpha=0.3, fill = "#ffa07a")+labs(y = "Electricity consumption ('000 GwH)",
  x = "Year",caption = paste("The solid blue line represents actual electricity consumption.",
  "The dashed red line represents the ARDL-predicted electricity consumption", sep = "\n"))+
  nogrid+theme(plot.caption = element_text(hjust=0))







