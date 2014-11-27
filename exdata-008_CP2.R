##init
readRDS("summarySCC_PM25.rds") -> NEI
readRDS("Source_Classification_Code.rds") -> scc
library(dplyr)
library(ggplot2)
library(ggthemes)


##plot1
NEI %>% group_by(year) %>% summarise(sum(Emissions)) %>% plot(col="red",type="l")

####re-write
NEI %>% group_by(year) %>% summarise(sum(Emissions)) -> p1
png(file="plot1.png")
plot(x = p1$Year,
     y = p1$SumEmission,
     col = "red",
     type = "l",
     xlab = "Year",
     ylab = "Total of Emissions",
     sub = "Total emissions from PM2.5 in the United States from 1999 to 2008",
     xlim = c(1998,2009),
     ylim = c(1800,3400)) 
dev.off()

##plot2
NEI %>% group_by(year) %>% filter(fips == "24510") %>% summarise(sum(Emissions)) -> p1 

plot(col="red",type="l")

##plot3
NEI %>% group_by(year,type) %>% filter(fips == "24510") %>% summarise(sum_Emi = sum(Emissions)) %>% ggplot(aes(year, sum_Emi)) + geom_line(colour= "red") + facet_grid(. ~ type)
####re-write
NEI %>% group_by(year,type) %>% filter(fips == "24510") %>% summarise(sum(Emissions)) -> p3
names(p3) <- c("Year","Type","SumEmissions")
p <- ggplot(p3, aes(x=Year,  y=SumEmissions, group=Type))
p <- p + geom_line(size=1, aes(colour=Type, group=Type))
p <- p + theme_gdocs()
png(file='plot3.png',width=950)
p
dev.off()


#plot4
scc[grep("Coal",scc$EI.Sector),'SCC'] -> coal
NEI %>% group_by(year) %>% filter(SCC %in% coal) %>%  summarise(sum_Emi_coal = sum(Emissions)) %>% plot(col="red",type="l")

#plot5
scc[grep("Vehicles",scc$EI.Sector),'SCC'] -> Vehicles
NEI %>% group_by(year) %>% filter(SCC %in% Vehicles,fips == "24510") %>%  summarise(sum_Emi_Vehicles = sum(Emissions)) %>% plot(col="red",type="l")

#plot6
scc[grep("Vehicles",scc$EI.Sector),'SCC'] -> Vehicles
NEI %>% group_by(year,fips) %>% filter(SCC %in% Vehicles,fips %in% c("24510","06037")) %>%  summarise(sum_Emi_Vehicles = sum(Emissions)) %>% ggplot(aes(year, sum_Emi_Vehicles)) + geom_line(colour= "red") + facet_grid(. ~ fips)
