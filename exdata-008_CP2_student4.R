###plot1

# load the necessary library
library(data.table)
# read the data from the current working directory
NEI <- readRDS("summarySCC_PM25.rds")
# convert to data table
DT <- data.table(NEI)
# Sum emissions by year
EBY <- DT[, sum(Emissions), by = year]
# plot emissions by year to the png device with a linear regression 
png(file = "plot1.png")
plot(EBY$year,EBY$V1, xlab='Year', ylab='Total Emissions')
model <- lm(V1 ~ year, EBY)
abline(model, lwd = 2)
dev.off()


###plot2

# load the necessary library
library(data.table)
# read the data from the current working directory
NEI <- readRDS("summarySCC_PM25.rds")
# convert to data table
DT <- data.table(NEI)
# subset to baltimore
baltimore <- subset(DT, fips == "24510")
# sum emissions by year
EBY <- baltimore[, sum(Emissions), by = year]
# plot emissions by year to the png device with a linear regression 
png(file = "plot2.png")
plot(EBY$year,EBY$V1, xlab='Year', ylab='Total Emissions in Baltimore')
model <- lm(V1 ~ year, EBY)
abline(model, lwd = 2)
dev.off()


###Plot3
# load the necessary library
library(data.table)
library(ggplot2)
# read the data from the current working directory
NEI <- readRDS("summarySCC_PM25.rds")
# convert to data table
DT <- data.table(NEI)
# subset to baltimore
baltimore <- subset(DT, fips == "24510")
# Sum emissions by year and type
EBY <- baltimore[, sum(Emissions), by = c('year','type')]
# plot emissions by year and type to the png device with a linear regression 
png(file = "plot3.png",width=960)
print(qplot(year,V1,data=EBY,facets=.~type,geom = c("point", "smooth"),method="lm"))
dev.off()


###Plot4
# load the necessary library
library(data.table)
library(ggplot2)
# read the data from the current working directory
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# find the SCC codes that are related to coal combustion
# by searching for the words comb and coal in the EI.Sector column
codes <- SCC[grep("[Cc]omb.*[Cc]oal",SCC$EI.Sector),"SCC"]
# convert to data table
DT <- data.table(NEI)
# subset to the codes related to coal cumbustion
coal <- subset(DT, SCC %in% codes)
# Sum emissions by year
EBY <- coal[, sum(Emissions), by = c('year')]
# plot emissions by year to the png device with a linear regression 
png(file = "plot4.png")
print(qplot(year,V1,data=EBY,geom = c("point", "smooth"),method="lm",
      ylab="Total Emissions related to coal combustion"))
dev.off()


###Plot5
# load the necessary library
library(data.table)
library(ggplot2)
# read the data from the current working directory
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# find the SCC codes that are related to motor vehicles
# by searching for the word Vehicle in the EI.Sector column
codes <- SCC[grep("Vehicle", SCC$EI.Sector), "SCC"]
# convert to data table
DT <- data.table(NEI)
# subset to Baltimore
baltimore <- subset(DT, fips == "24510")
# subset to motor vehicles
bmv <- subset(baltimore, SCC %in% codes)
# Sum emissions by year
EBY <- bmv[, sum(Emissions), by = c('year')]
# plot emissions by year to the png device with a linear regression 
png(file = "plot5.png")
print(qplot(year,V1,data=EBY,geom = c("point", "smooth"),method="lm",
      ylab="Total Emissions from Motor Vehicles in Baltimore"))
dev.off()


###Plot6
# load the necessary library
library(data.table)
library(ggplot2)
library(dplyr)
# read the data from the current working directory
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# find the SCC codes that are related to motor vehicles
# by searching for the word Vehicle in the EI.Sector column
codes <- SCC[grep("Vehicle", SCC$EI.Sector), "SCC"]
# convert to data table
DT <- data.table(NEI)
# subset to Baltimore and Los Angeles
baltimore <- subset(DT, fips == "24510" | fips == "06037")
# subset to motor vehicles
bmv <- subset(baltimore, SCC %in% codes)
# Sum emissions by year
EBY <- bmv[, sum(Emissions), by = c("year","fips")]
# Change a City column to contain the names of the two cities
EBY <- mutate(EBY,City = ifelse(fips == 24510, "Baltimore", "Los Angeles"))
# plot emissions by year to the png device with a linear regression 
png(file = "plot6.png")
print(qplot(year,V1,data=EBY,color = City,geom = c("point", "smooth"),method="lm",
      ylab="Total Emissions from Motor Vehicles"))
dev.off()

