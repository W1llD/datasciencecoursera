#p1
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
agg <- aggregate(NEI$Emissions, by = list(year=NEI$year), FUN = sum, na.rm=TRUE)


png(file="plot1.png",height=480,width=480)
par(mar=c(5,7,5,2))
plot(agg$year, agg$x, type="n", xlab="Year", axes=F, ylab="")
lines(as.numeric(agg$x)~agg$year, col="red", lwd=2)
box()
axis(side=1, at=c(1999,2002,2005,2008))
axis(side=2, at=agg$x, las = 1)
title("Emissions per Year: 1999 to 2008")
mtext(side = 2, "Total Emissions (tons)", line=5)
dev.off()

#p2
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
emBaltimore <- subset(NEI, NEI$fips=="24510")
agg <- aggregate(emBaltimore$Emissions, by = list(year=emBaltimore$year), FUN = sum, na.rm=TRUE)


png(file="plot2.png",height=480,width=480)
par(mar=c(5,7,5,2))
plot(agg$year, agg$x, type="n", xlab="Year", axes=F, ylab="")
lines(as.numeric(agg$x)~agg$year, col="red", lwd=2)
box()
axis(side=1, at=c(1999,2002,2005,2008))
axis(side=2, at=agg$x, las = 1)
title("Emissions per Year in Baltimore: 1999 to 2008")
mtext(side = 2, "Total Emissions (tons)", line=5)
dev.off()

#p3
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
emBaltimore <- subset(NEI, NEI$fips=="24510")
agg <- transform(emBaltimore, year=factor(year), type=factor(type))
agg1 <- aggregate(cbind(Emissions) ~ year + type, sum, data=agg)




png(file="plot3.png",height=480,width=640)
qplot(year, Emissions, data = agg1, facets = . ~ type, ylab="Emissions (tons)", xlab="Year", main="Emissions per type and year in Baltimore") + geom_line(aes(colour = Emissions, group = type))
dev.off()

#p4
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
filteredSCC <- subset(SCC, grepl("Coal", Short.Name))
usaCoal <- subset(NEI, NEI$SCC %in% filteredSCC$SCC)
agg <- transform(usaCoal, year=factor(year))
agg1 <- aggregate(cbind(Emissions) ~ year, sum, data=agg)




png(file="plot4.png",height=480,width=640)
qplot(year, Emissions, data = agg1, ylab="Emissions (tons)", xlab="Year", main="Coal Emissions in USA per Year") + geom_line(aes(colour = Emissions, group = 1))
dev.off()

#p5
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
filteredSCC <- subset(SCC, grepl("Vehicle", EI.Sector))
usaVeh <- subset(NEI, NEI$SCC %in% filteredSCC$SCC)
usaVehBalt <- subset(usaVeh, usaVeh$fips=="24510")
agg <- transform(usaVehBalt, year=factor(year))
agg1 <- aggregate(cbind(Emissions) ~ year, sum, data=agg)




png(file="plot5.png",height=480,width=640)
qplot(year, Emissions, data = agg1, ylab="Emissions (tons)", xlab="Year", main="Vehicle Motor Emissions in Baltimore per Year") + geom_line(aes(colour = Emissions, group = 1))
dev.off()

#p6
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
filteredSCC <- subset(SCC, grepl("Vehicle", EI.Sector))
usaVeh <- subset(NEI, NEI$SCC %in% filteredSCC$SCC)
usaVehBaltLos <- subset(usaVeh, usaVeh$fips=="24510" | usaVeh$fips=="06037")
usaVehBaltLos <- transform(usaVehBaltLos,
                fips = ifelse(fips == "24510", "Baltimore City", "Los Angeles County"))
agg <- transform(usaVehBaltLos, year=factor(year), fips=factor(fips))
agg1 <- aggregate(cbind(Emissions) ~ year + fips, sum, data=agg)




png(file="plot6.png",height=480,width=640)
qplot(year, Emissions, data = agg1, ylab="Emissions (tons)", xlab="Year", main="Vehicle Motor Emissions in Baltimore x L.A. per Year") + geom_line(aes(colour = fips, group = fips)) + scale_colour_hue(name="Cities")
dev.off()
