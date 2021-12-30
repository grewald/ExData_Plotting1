#load required libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
# download , unzip and read
temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip",temp)
allElectricUsage <- read_table2(unz(temp, "household_power_consumption.txt"))
allElectricUsage <- read_table(unz(temp, "household_power_consumption.txt"))
allElectricUsage <- read.table(unz(temp, "household_power_consumption.txt"), sep = ';')

# correctly name the data columns
colnames(allElectricUsage) <- allElectricUsage[1,]
# remove the first row, that contains variable names
nrow <- nrow(allElectricUsage)
allElectricUsage <- allElectricUsage [2:nrow,] 


# create date variable and date time variable and keep only 2 day recode
s2 <- allElectricUsage %>%  mutate(dateR= dmy(Date ) , 
                                   datetimeR = as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S")) %>% 
  filter("2007-02-01" ==  dateR  | "2007-02-02" ==  dateR)

# convert  Global_active_power to numeric
s2 <- s2 %>%  mutate(Global_active_power_n = as.numeric(Global_active_power))


# generate plot4-1   
p1 <- s2 %>%  ggplot(aes(x= datetimeR, y= Global_active_power_n)) + geom_line() + 
  labs(y= "Global Active Power (Killowats)", x= "Datetime")
#generate plot 4-2

toPlot1 <- s2 %>% select(starts_with('Sub_metering'), datetimeR) %>% 
  pivot_longer(cols = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))


p2 <- toPlot1 %>%  ggplot(aes(x= datetimeR,y=as.numeric(value), color= name)) + geom_line()+
  labs(x= "Datetime", y= "Energy Sub Metering" ) +
  theme(legend.title = element_blank(), legend.position = c(.9,.9) )

#generate plot 4

#create the two new plots first
# plot 4-3
toPlot2 <- s2 %>% mutate(volt_n= as.numeric(Voltage))
p3 <- toPlot2 %>%  ggplot(aes(x= datetimeR, y= volt_n)) + geom_line() + 
  labs(y= "Voltage", x= "Datetime") +
  scale_y_continuous(limits=c(230,250), breaks=c(230, 240,250,300))

#plot 4-2

p4 <- s2 %>%  ggplot(aes(x= datetimeR, y= as.numeric(Global_reactive_power))) + geom_line() + 
  labs( x= "Datetime", y= "Global_reactive_power")

par(mfrow= c(2,2))
with(s2,{
   ggplot(s2, aes(x= datetimeR, y= Global_active_power_n)) + geom_line() + 
    labs(y= "Global Active Power (Killowats)", x= "Datetime")
   ggplot(toPlot1, aes(x= datetimeR,y=as.numeric(value), color= name)) + geom_line()+
    labs(x= "Datetime", y= "Energy Sub Metering" ) +
    theme(legend.title = element_blank(), legend.position = c(.9,.9) )
   ggplot(toPlot2, aes(x= datetimeR, y= volt_n)) + geom_line() + 
     labs(y= "Voltage", x= "Datetime") +
     scale_y_continuous(limits=c(230,250), breaks=c(230, 240,250,300))
   ggplot(s2, aes(x= datetimeR, y= as.numeric(Global_reactive_power))) + geom_line() + 
     labs( x= "Datetime", y= "Global_reactive_power")
   
   
  })


ggsave("plot4.png",  width = 480, height = 480, units = c("px"), dpi= "screen") 
 
