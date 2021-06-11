#Redoing Figure 3

# Create 4 variables
group <- c(rep("Philippines", 4), rep("Rural", 4), rep("Urban", 4))
value <- c(65, 71, 83, 88, 46, 52, 73, 82, 86, 92, 94, 94)
year <- c(rep(c(1990, 2000, 2010, 2012),3))
label1 <- c(rep("", 11), "Urban")
label2

df.fig3 <- data.frame(group, value, year, label1)

source("nogrid.R")
ggplot(data = df.fig3)+geom_line(aes(x = year, y = value, linetype= group, color = group))+
  labs(y = "% of population with access to electricity", x = "Year")+ 
  geom_label(aes(x = 2011, y = 97, label = "Urban"), label.size = NA, fill = "white")+
  geom_label(aes(x = 2011, y = 90, label = "Philippines"), label.size = NA, fill = "white")+
  geom_label(aes(x = 2012, y = 84, label = "Rural"), label.size = NA, fill = "white")+nogrid+
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2012))+
  geom_point(aes(x = year, y= value, color = group))+theme(legend.position = "none")


#Redoing Figure 2
library("readxl")
library(reshape2)
library(ggrepel)
sectoral <- read_excel("Sectoral-Demand.xlsx")
sectoral <- melt(sectoral, id.vars="Year")
sectoral <- sectoral[sectoral$variable != "Total Sales",]

sectoral$variable<- factor(sectoral$variable, levels = c("Others", "Commercial",
                    "Industrial", "Residential"))

Text_year <- subset(sectoral, sectoral$Year==2015)

ggplot(sectoral, aes(x=Year, y=value, fill=variable)) + 
  geom_area(alpha=0.6 , size=.5, colour="white") +
  labs(y = "Electricity consumption in GwH", fill = "Customer group")+ nogrid+
  geom_label(aes(x = 2010.2, y = 61500), label = "Others", size = 2.8, label.size = NA, fill = "#e78ac3")+
  geom_label(aes(x = 2010.2, y = 44300), label = "Commercial", size = 2.8, label.size = NA, fill = "#8da0cb")+
  geom_label(aes(x = 2010.2, y = 26000), label = "Industrial", size= 2.8, label.size = NA, fill = "#fc8d62")+
  geom_label(aes(x = 2010.2, y = 7000), label = "Residential", size = 2.8, label.size = NA, fill = "#66c2a5")+
  scale_fill_manual(values = c("#e78ac3", "#8da0cb", "#fc8d62", "#66c2a5"))+theme(legend.position = "none")
  
  