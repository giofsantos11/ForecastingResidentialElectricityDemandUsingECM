#####################################################################
###Performing the ECM for training data and plotting test-set prediction
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

##Create dataframe
df1 = data.frame(Dln_cons, Dln_hhfe, LDln_price, Dln_temp, Lln_cons, Lln_hhfe)

##Create training subset: hold out period for 2012 - 2015 (4 years)
train = 1:(nrow(HHElec)-5)

##Perform one-stage ECM for training data
model.ecm.train <- lm(Dln_cons ~ Dln_hhfe+ LDln_price + Dln_temp + Lln_cons + Lln_hhfe,
                data = df1[train,])

summary(model.ecm.train)

##Predict point estimates and intervals
pred.ecm.test<- predict(model.ecm.train, df1[-train,], interval = "confidence")

##Merge predicted values with training data 
y <- data.frame(fit=rep(NA,18), lwr = rep(NA,18),upr = rep(NA,18))
x<- data.frame(pred.ecm.test)
z <- rbind(y,x)
df.cons.test <- cbind(df1, z)
df.cons.test <- cbind(df.cons.test, HHElec$year[2:23])

##Rename the variables in the created data frame
colnames(df.cons.test) <- c("Dln_cons", "Dln_hhfe", "LDln_price","Dln_temp",
                        "Lln_cons", "Lln_hhfe", "fit", "lower", "upper", "year")

##Merge complete years dataset
df.full <- right_join(df.cons.test, HHElec, by="year")
df.full <- df.full[order(df.full$year),]

##Transform point estimate in levels
df.full$ln.cons.sim<- df.full$fit + df.full$Lln_cons
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

df.full$cons.sim.lab[1:19]=df.full$hhcons.lab[1:19]

##Create line graph (within sample)
ggplot(data = df.full)+geom_line(aes(x = year, y = cons.sim.lab), color = "#ef8a62",
  linetype = "dashed")+geom_line(aes(x = year, y = hhcons.lab), color="#67a9cf")+
  geom_ribbon(aes(ymin=low.cons.sim.lab,ymax=up.cons.sim.lab, x = year),
  alpha=0.3, fill = "#ffa07a")+labs(y = "Electricity consumption ('000 GwH)",
  x = "Year",caption = paste("The solid blue line represents actual electricity consumption.",
  "The dashed red line represents the ECM-predicted electricity consumption", sep = "\n"))+
  nogrid+theme(plot.caption = element_text(hjust=0))



