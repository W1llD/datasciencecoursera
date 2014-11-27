##init
library(dplyr)
library(ggplot2)
library(ggthemes)


readRDS("summarySCC_PM25.rds") -> NEI
readRDS("Source_Classification_Code.rds") -> scc

##plot1
###NEI %>% group_by(year) %>% summarise(sum(Emissions)) %>% plot(col="red",type="l")
####re-write
NEI %>% group_by(year) %>% summarise(sum(Emissions)) -> p1
c("Year","SumEmission") -> names(p1)

png(file="plot1.png")
plot(x = p1$Year,
     y = p1$SumEmission,
     col = "red",
     type = "l",
     xlab = "Year",
     ylab = "Total of Emissions",
     sub = "Total PM2.5 Emissions in the United States between 1999-2008",
     xlim = c(1998,2009)) 
dev.off()

##plot2
NEI %>% group_by(year) %>% filter(fips == "24510") %>% summarise(sum(Emissions)) -> p2
c("Year","SumEmission") -> names(p2)

png(file="plot2.png")
plot(x = p2$Year,
     y = p2$SumEmission,
     col = "red",
     type = "l",
     xlab = "Year",
     ylab = "Total of Emissions",
     sub = "Total PM2.5 Emissions in the Baltimore City between 1999-2008",
     xlim = c(1998,2009)) 
dev.off()

##plot3
###NEI %>% group_by(year,type) %>% filter(fips == "24510") %>% summarise(sum_Emi = sum(Emissions)) %>% ggplot(aes(year, sum_Emi)) + geom_line(colour= "red") + facet_grid(. ~ type)
####re-write
NEI %>% group_by(year,type) %>% filter(fips == "24510") %>% summarise(sum(Emissions)) -> p3
c("Year","Type","SumEmissions") -> names(p3) 

p <- ggplot(p3, aes(x=Year,  y=SumEmissions, group=Type)) + 
     geom_line(size=1, aes(colour=Type, group=Type)) + 
     ggtitle('Total Emissions in Baltimore.') + 
     theme_gdocs()
png(file='plot3.png')
p
dev.off()


#plot4
scc[grep("Coal",scc$EI.Sector),'SCC'] -> coal
###NEI %>% group_by(year) %>% filter(SCC %in% coal) %>%  summarise(sum_Emi_coal = sum(Emissions)) %>% plot(col="red",type="l")
NEI %>% group_by(year) %>% filter(SCC %in% coal) %>%  summarise(sum(Emissions)) -> p4
c("Year","SumEmission") -> names(p4)

png(file="plot4.png")
plot(x = p4$Year,
     y = p4$SumEmission,
     col = "red",
     type = "l",
     xlab = "Year",
     ylab = "Total of Emissions",
     sub = "Total Emissions from Coal in US between 1999-2008",
     xlim = c(1998,2009)) 
dev.off()



#plot5
scc[grep("Vehicles",scc$EI.Sector),'SCC'] -> Vehicles
###NEI %>% group_by(year) %>% filter(SCC %in% Vehicles,fips == "24510") %>%  summarise(sum_Emi_Vehicles = sum(Emissions)) %>% plot(col="red",type="l")

NEI %>% group_by(year) %>% filter(SCC %in% Vehicles,fips == "24510") %>%  summarise(sum(Emissions)) -> p5
c("Year","SumEmission") -> names(p5)

png(file="plot5.png")
plot(x = p5$Year,
     y = p5$SumEmission,
     col = "red",
     type = "l",
     xlab = "Year",
     ylab = "Total of Emissions",
     sub = "Total Emissions from Vehicle in Baltimore City between 1999-2008",
     xlim = c(1998,2009),
     ylim = c(80,350)) 
dev.off()


#plot6
scc[grep("Vehicles",scc$EI.Sector),'SCC'] -> Vehicles
##NEI %>% group_by(year,fips) %>% filter(SCC %in% Vehicles,fips %in% c("24510","06037")) %>%  summarise(sum_Emi_Vehicles = sum(Emissions)) %>% ggplot(aes(year, sum_Emi_Vehicles)) + geom_line(colour= "red") + facet_grid(. ~ fips)
NEI %>% group_by(year,fips) %>% filter(SCC %in% Vehicles,fips %in% c("24510","06037")) %>%  summarise(sum_Emi_Vehicles = sum(Emissions)) -> p6
c("Year","Fips","SumEmissions") -> names(p6)

p <- ggplot(p6, aes(x=Year,  y=SumEmissions, group=Fips)) + 
     ggtitle('Total Emissions from vehicle in Baltimore vs LA County.') + 
     geom_line(size=1, aes(colour=Fips, group=Fips))    + 
     theme_gdocs() 
png(file='plot6.png')
p
dev.off()
