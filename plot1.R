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

# generate plot
s2 %>%  ggplot(aes(Global_active_power_n)) + geom_histogram(fill='red',bins = 30) + 
  labs(x= "Global Active Power (Killowats)", y= "Frequency")
   

               