# This script creates the first plot file for the 
# Exploratory Data Analysis - Course Project #2)

# Note: data files (Source_Classification_Code.rds & summarySCC_PM25.rds) must be in your working directory

# Add required packages
library(dplyr)

# Extract and read files 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Group and Sum by year
byYear <- group_by(NEI, year)
SumByYear <- summarise(byYear, sum(Emissions))

# Change column names
colnames(SumByYear) <- c("year","SumEmissions")

# Create plot showing total PM2.5 emissions by year
# and save to file
png(filename = "plot1.png", width=480, height=480, units="px")
plot(SumByYear, xlab="Year", ylab="Total PM2.5 Emissions", type="b")
dev.off()


# This script creates the second plot file for the 
# Exploratory Data Analysis - Course Project #2)

# Note: data files (Source_Classification_Code.rds & summarySCC_PM25.rds) must be in your working directory

# Add required packages
library(dplyr)

# Extract and read files 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Create subset of Baltimore data only
tdNEI <- tbl_df(NEI)
tdBaltimore <- filter(tdNEI, fips=="24510")

# Group and Sum by year
byYear <- group_by(tdBaltimore, year)
SumByYearBaltimore <- summarise(byYear, sum(Emissions))

# Change column names
colnames(SumByYear) <- c("year","SumEmissions")

# Create plot showing Baltimore city PM2.5 emissions from 1998-2005
# and save to file
png(filename = "plot2.png", width=480, height=480, units="px")
plot(SumByYearBaltimore, xlab="Year", ylab="Total PM2.5 Emissions", type="b")
dev.off()



# This script creates the third plot file for the 
# Exploratory Data Analysis - Course Project #2)

# Note: data files (Source_Classification_Code.rds & summarySCC_PM25.rds) must be in your working directory

# Add required packages
library(dplyr)
library(ggplot2)

# Extract and read files 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Create subset of Baltimore data only
tdNEI <- tbl_df(NEI)
tdBaltimore <- filter(tdNEI, fips=="24510")

# Group and Sum by year and emissions type
byYearSource <- group_by(tdBaltimore, year, type)
SumByYearSourceBaltimore <- summarise(byYearSource, sum(Emissions))

# Change column names
colnames(SumByYearSourceBaltimore) <- c("year", "type", "PM2.5")

# Create plot showing Baltimore PM2.5 emissions by source and year
# and save to file
png(filename = "plot3.png", width=480, height=480, units="px")
g <- ggplot(SumByYearSourceBaltimore, aes(year, PM2.5, color=type))
g + geom_line()
# cound also use the easier qplot function to produce the same plot:
#   qplot(year, PM2.5, data = SumByYearSourceBaltimore, color=type, geom="line")
dev.off()


# This script creates the fourth plot file for the 
# Exploratory Data Analysis - Course Project #2)

# Note: data files (Source_Classification_Code.rds & summarySCC_PM25.rds) must be in your working directory

# Add required packages
library(dplyr)

# Extract and read files 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
tdNEI <- tbl_df(NEI)

# Search for coal combustion sources in the SCC dataset
# find "coal" in Short.Name colum
temp <- as.data.frame(SCC[grep("[Cc]oal", SCC$Short.Name),])#"Short.Name"])
#removing 'charcoal' entries from subset
temp <- temp[-grep("[Cc]harcoal", temp$Short.Name), ]
#find combustions sources within the coal subset and create the final coal combustion subset
temp2 <- temp[grep("[Cc]omb", temp$Short.Name), ]
CoalCombSCC <- temp2[, 1:3]

# Join the NEI dataset to the coal combustion subset and create new dataset
# with just Coal Combustion sources
CoalCombEmissions <- merge(tdNEI, CoalCombSCC, by.x = "SCC", by.y = "SCC")

# Group and Sum by Year
byYear <- group_by(CoalCombEmissions, year)
SumByYearCoalComb <- summarise(byYear, sum(Emissions))
colnames(SumByYearCoalComb) <- c("Year","SumEmissions")

# Create plot showing emissions from coal combustion-related from 
# across the United States during 1999-2008
# and save to file
png(filename = "plot4.png", width=480, height=480, units="px")
plot(SumByYearCoalComb, xlab="Year", ylab="Total PM2.5 Emissions", type="b", main="Coal Combustion, United States")
dev.off()


# This script creates the fifth plot file for the 
# Exploratory Data Analysis - Course Project #2)

# Note: data files (Source_Classification_Code.rds & summarySCC_PM25.rds) must be in your working directory

# Add required packages
library(dplyr)

# Extract and read files 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
tdNEI <- tbl_df(NEI)

# Search for motor vehicle sources in the SCC dataset
# find "vehicle" in EI.Sector column.  Assumption is this is the best way
# to extract motor vehicle SCC codes from the SCC table
temp2 <- SCC[grep("[Vv]ehicle", SCC$EI.Sector),]
MVSCC <- temp2[, 1:3]

# Create subset of Baltimore data only from the NEI dataset
tdBaltimore <- filter(tdNEI, fips=="24510")

# Join subetted NEI dataset (tdBaltimore) to the motor vehicle subset and create new dataset
MVBaltEmissions <- merge(tdBaltimore, MVSCC, by.x = "SCC", by.y = "SCC")

# Group and Sum by Year
byYear <- group_by(MVBaltEmissions, year)
SumByYearMVBalt <- summarise(byYear, sum(Emissions))
colnames(SumByYearMVBalt) <- c("Year","SumEmissions")

# Create plot showing how emmissions from motor vehicels sources have changed
# from 1999-2008 in Baltimore City
# and save to file
png(filename = "plot5.png", width=480, height=480, units="px")
plot(SumByYearMVBalt, xlab="Year", ylab="Total PM2.5 Emissions", type="b", main="Motor Vehicle Emissions, Baltimore")
dev.off()

#Additional notes:
# The EI.Sector column was chosen over the Short.Name column to select motor vehicle SCC 
# codes because the data appeared to be a more accurate representation of what was being
# asked by the question.  Sum of emissions is show below for comparison

# Using Short.Name, and searching for "vehicle":
# Year SumEmissions
# 1 1999     72.52000
# 2 2002     38.72593
# 3 2005     34.73461
# 4 2008     24.62275

# Using EI.Sector, and searching for "vehicle":
# Year SumEmissions
# 1 1999    346.82000
# 2 2002    134.30882
# 3 2005    130.43038
# 4 2008     88.27546


# This script creates the sixth plot file for the 
# Exploratory Data Analysis - Course Project #2)

# Note: data files (Source_Classification_Code.rds & summarySCC_PM25.rds) must be in your working directory

# Add required packages
library(dplyr)

# Extract and read files 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
tdNEI <- tbl_df(NEI)

# Search for motor vehicle sources in the SCC dataset
# find "vehicle" in EI.Sector column.  Assumption is this is the best way
# to extract motor vehicle SCC codes from the SCC table
temp2 <- SCC[grep("[Vv]ehicle", SCC$EI.Sector),]
MVSCC <- temp2[, 1:3]

# Create subset of Baltimore data only from the NEI dataset
tdBaltimore <- filter(tdNEI, fips=="24510")
# Join subsetted NEI dataset (tdBaltimore) to the motor vehicle subset and create new dataset
MVBaltEmissions <- merge(tdBaltimore, MVSCC, by.x = "SCC", by.y = "SCC")

# Create subset of Los Angeles data only from the NEI dataset
tdLA <- filter(tdNEI, fips=="06037")
# Join subsetted NEI dataset (tdLA) to the motor vehicle subset and create new dataset
MVLAEmissions <- merge(tdLA, MVSCC, by.x = "SCC", by.y = "SCC")

# Group each city-specific subset and Sum by Year
byYearBalt <- group_by(MVBaltEmissions, year)
SumByYearMVBalt <- summarise(byYearBalt, sum(Emissions))
colnames(SumByYearMVBalt) <- c("Year","SumEmissions")

byYearLA <- group_by(MVLAEmissions, year)
SumByYearMVLA <- summarise(byYearLA, sum(Emissions))
colnames(SumByYearMVLA) <- c("Year","SumEmissions")

# Perform a couple of basic calculations (actual change and % change) to help determine 
# how to answer the question about which location has seen the greater change over time --
BaltChange <- as.integer(SumByYearMVBalt[4,2] - SumByYearMVBalt[1,2])
BaltPerChange <- BaltChange/SumByYearMVBalt[1,2]
LAChange <- as.integer(SumByYearMVLA[4,2] - SumByYearMVLA[1,2])
LAPerChange <- LAChange/SumByYearMVLA[1,2]

# Create plot showing which location (Baltimore City or L.A. County saw 
# greater changes in emmissions from motor vehicels sources over the 1999-2008 period
# and save to file
png(filename = "plot6.png", width=640, height=480, units="px")
par(mfrow = c(1,2))
plot(SumByYearMVBalt, xlab="Year", ylab="Total PM2.5 Emissions", type="b"
     , main="Motor Vhcl Emissions, Baltimore")
text(SumByYearMVBalt[3,1], SumByYearMVBalt[3,2]+60, "Total Change")
text(SumByYearMVBalt[3,1]+1, SumByYearMVBalt[3,2]+40, BaltChange)
text(SumByYearMVBalt[3,1]+1, SumByYearMVBalt[3,2]+20, 
     paste(round(BaltPerChange*100, digits=1),"%", sep=""))

plot(SumByYearMVLA, xlab="Year", ylab="Total PM2.5 Emissions", type="b"
     , main="Motor Vhcl Emissions, LA County")
text(SumByYearMVLA[3,1], SumByYearMVLA[3,2]-500, "Total Change")
text(SumByYearMVLA[3,1], SumByYearMVLA[3,2]-550, LAChange)
text(SumByYearMVLA[3,1], SumByYearMVLA[3,2]-600, 
     paste(round(LAPerChange*100, digits=1),"%",sep=""))
dev.off()


#Additional notes:
# The EI.Sector column was chosen over the Short.Name column to select motor vehicle SCC 
# codes because the data appeared to be a more accurate representation of what was being
# asked by the question.  Sum of emissions is show below for comparison

# Using Short.Name, and searching for "vehicle":
# Year SumEmissions
# 1 1999     72.52000
# 2 2002     38.72593
# 3 2005     34.73461
# 4 2008     24.62275

# Using EI.Sector, and searching for "vehicle":
# Year SumEmissions
# 1 1999    346.82000
# 2 2002    134.30882
# 3 2005    130.43038
# 4 2008     88.27546


