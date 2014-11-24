# This program needs 2 data files - summarySCC_PM25.rds and Source_Classification_Code.rds
# PM2.5 Emissions Data (summarySCC_PM25.rds): 
# This file contains a data frame with all of the PM2.5 emissions data 
# for 1999, 2002, 2005, and 2008. 
# Source Classification Code Table (Source_Classification_Code.rds): 
# This table provides a mapping from the SCC digit strings int he Emissions 
# table to the actual name of the PM2.5 source. 
#
# Specify the file names and the location where they exist

DataDir="/Users/vijjisekuboyina/Coursera/Course4-Explore/data"
File_NEI = sprintf("%s/%s", DataDir, "summarySCC_PM25.rds")
File_SCC = sprintf ("%s/%s", DataDir, "Source_Classification_Code.rds")

# Both the files are read using the readRDS() function in R.
NEI <- readRDS(File_NEI)
SCC <- readRDS(File_SCC)

if (is.null(NEI) || is.null(SCC)) {
  stop ("Invalid Data Files.....Exiting")
}

# Question 1:
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission 
# from all sources for each of the years 1999, 2002, 2005, and 2008.

# Find total PM2.5 emission for all 4 years
TotEmissionPerYr <- aggregate (Emissions ~ year, NEI, sum)

# Open the plot file to save the plot
png ("plot1.png", width=500, height=500, units="px", bg="white")

# Draw the Plot for PM2.5 emissions for each year. Since the totals are very high,
# Scale them down by dividing with 10,0000

barplot (TotEmissionPerYr$Emissions/10000, 
        names.arg=TotEmissionPerYr$year, 
        xlab="Year", 
        ylab="PM 2.5 Emissions [in 10,000 Tons]",
        main="PM 2.5 Emissions from all sources",
        pch=18,
        col="red")

# Close the plot file
dev.off()

# Question 2:
# Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

# Find total PM2.5 emission for Baltimore City [fips=24510] from 1999 to 2008

TotEmissionsinBalt <- aggregate (Emissions ~ year, NEI[NEI$fips=="24510",], sum)

# Open the plot file to save the plot
png ("plot2.png", width=500, height=500, units="px", bg="white")

# Draw the Plot for PM2.5 emissions for each year. Since the totals are very high,
# Scale them down by dividing with 100000

barplot (TotEmissionsinBalt$Emissions/10000, 
        names.arg=TotEmissionsinBalt$year, 
        xlab="Year", 
        ylab="PM 2.5 Emissions [in 10,000 Tons]",
        main="PM 2.5 Emissions from all sources in Baltimore City",
        pch=18,
        col="blue")

# Close the plot file
dev.off()

# Question 3:
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
# variable, which of these four sources have seen decreases in emissions from 1999–2008
# for Baltimore City? Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.


# Identify the Baltimore City [fips=24510] NEI
BaltNEI <- NEI[NEI$fips=="24510",]

# Load gglplot2
library(ggplot2)

# Open the plot file to save the plot
png ("plot3.png", width=750, height=500, units="px", bg="white")

# Draw the Plot for PM2.5 emissions for all years for each source

GP <- ggplot(BaltNEI, aes(factor(year), Emissions, fill=type)) + 
              geom_bar(stat="identity") +
              facet_grid(.~type) +
              theme_bw() +
              theme (legend.position="bottom", legend.box="horizontal") +
              theme (legend.title=element_blank()) +
              labs(x="Year", y="PM 2.5 Emissions",title="PM 2.5 Emissions in Baltimore by Source Type")

print (GP)
# Close the plot file
dev.off()

# Question 4: Across the United States, how have emissions from 
# coal combustion-related sources changed from 1999–2008?

# Find SCC data for Coal and Combusion
# Comb - SCC records where SCC.Level.One contains "Comb"
# Coal - SCC record where SCC.Level.Four contains "Coal"
TotalComb <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
TotalCoal <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE)

# Combine both Coal and Combustion and identify SCC and NEI data for them
CoalComb <- (TotalComb & TotalCoal)
CoalCombSCC <- SCC[CoalComb,]$SCC
CoalCombNEI <- NEI[NEI$SCC %in% CoalCombSCC,]

# Find aggregates per year
TotalCoalCombPerYr <- aggregate (Emissions ~ year, CoalCombNEI, sum)

# Open the plot file to save the plot
png ("plot4.png", width=500, height=500, units="px", bg="white")

# Draw the Plot for PM2.5 emissions for each year for Coal and Combustion.
# Since the totals are very high, Scale them down by dividing with 100000

barplot (TotalCoalCombPerYr$Emissions/10000, 
        names.arg=TotalCoalCombPerYr$year, 
        xlab="Year", 
        ylab="PM 2.5 Emissions [in 10,000 Tons]",
        main="PM 2.5 Coal & Combustion Emissions from all sources",
        pch=18,
        col="green")

# Close the plot file
dev.off()

# Question 5: How have emissions from motor vehicle sources changed
# from 1999–2008 in Baltimore City? 

# First we find data for motor vehicles [SCC.Level.Two]
# and identify the SCC and NEI data
MVData <- grepl ("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
MVSCC <- SCC[MVData,]$SCC
MVNEI <- NEI[NEI$SCC %in% MVSCC,]

# Find MV in Baltimore [fips==24510]
BaltMVNEI <- MVNEI[MVNEI$fips=="24510",]

# Find aggregates per year
TotBaltEmissionsPerYr <- aggregate (Emissions ~ year, BaltMVNEI, sum)

# Open the plot file to save the plot
png ("plot5.png", width=500, height=500, units="px", bg="white")

# Draw the Plot for PM2.5 emissions for each year for Motor Vehicles

barplot (TotBaltEmissionsPerYr$Emissions, 
        names.arg=TotBaltEmissionsPerYr$year, 
        xlab="Year", 
        ylab="PM 2.5 Emissions",
        main="PM 2.5 Motor Vehicle Emissions in Baltimore from all sources",
        pch=18,
        col="yellow")

# Close the plot file
dev.off()

# Question 6: Compare emissions from motor vehicle sources in 
# Baltimore City with emissions from motor vehicle sources in Los Angeles County, 
# California (fips == "06037"). Which city has seen greater changes over time in
# motor vehicle emissions?

# First we find data for motor vehicles [SCC.Level.Two]
# and identify the SCC and NEI data
MVData <- grepl ("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
MVSCC <- SCC[MVData,]$SCC
MVNEI <- NEI[NEI$SCC %in% MVSCC,]

# Find MV in Baltimore [fips==24510] and MV in LosAngeles [fips==06037]
BaltMVNEI <- MVNEI[MVNEI$fips=="24510",]
BaltMVNEI$city <- "Baltimore City"
LAMVNEI <- MVNEI[MVNEI$fips=="06037",]
LAMVNEI$city <- "Los Angeles City"

# Combine data of both the cities
Balt_LA_MVNEI <- rbind(BaltMVNEI, LAMVNEI)

# Open the plot file to save the plot
png ("plot6.png", width=500, height=500, units="px", bg="white")

# Draw the Plot for PM2.5 emissions for each year for both LA and Baltimore
# Using ggplot2
library(ggplot2)

GP <- ggplot(Balt_LA_MVNEI, aes(factor(year), Emissions, fill=city)) +
            geom_bar(stat="identity") +
            facet_grid(.~city) +
            theme_bw() +
            theme (legend.position="bottom", legend.box="horizontal") +
            theme (legend.title=element_blank()) +
            labs(x="Year", y="PM 2.5 Emissions",title="PM 2.5 Emissions from Motor Vehicles")

print (GP)

# Close the plot file
dev.off()

