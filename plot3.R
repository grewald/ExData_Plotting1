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


#generate plot 3

toPlot <- s2 %>% select(starts_with('Sub_metering'), datetimeR) %>% 
  pivot_longer(cols = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))


toPlot %>%  ggplot(aes(x= datetimeR,y=as.numeric(value), color= name)) + geom_line()+
  labs(x= "Datetime", y= "Energy Sub Metering" ) +
  theme(legend.title = element_blank(), legend.position = c(.9,.9) )


ggsave("plot3.png",  width = 480, height = 480, units = c("px"), dpi= "screen") 
