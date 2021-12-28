
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
