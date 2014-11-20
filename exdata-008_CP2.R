##init
readRDS("summarySCC_PM25.rds") -> NEI
readRDS("Source_Classification_Code.rds") -> scc
library(dplyr)
library(ggplot2)

##plot1
NEI %>% group_by(year) %>% summarise(sum(Emissions)) %>% plot(col="red",type="l")

##plot2
NEI %>% group_by(year) %>% filter(fips == "24510") %>% summarise(sum(Emissions)) %>% plot(col="red",type="l")

##plot3
NEI %>% group_by(year,type) %>% filter(fips == "24510") %>% summarise(sum_Emi = sum(Emissions)) %>% ggplot(aes(year, sum_Emi)) + geom_line(colour= "red") + facet_grid(. ~ type)

#plot4
scc[grep("Coal",scc$EI.Sector),'SCC'] -> coal
NEI %>% group_by(year) %>% filter(SCC %in% coal) %>%  summarise(sum_Emi_coal = sum(Emissions)) %>% plot(col="red",type="l")

#plot5
scc[grep("Vehicles",scc$EI.Sector),'SCC'] -> Vehicles
NEI %>% group_by(year) %>% filter(SCC %in% Vehicles,fips == "24510") %>%  summarise(sum_Emi_Vehicles = sum(Emissions)) %>% plot(col="red",type="l")

#plot6
scc[grep("Vehicles",scc$EI.Sector),'SCC'] -> Vehicles
NEI %>% group_by(year,fips) %>% filter(SCC %in% Vehicles,fips %in% c("24510","06037")) %>%  summarise(sum_Emi_Vehicles = sum(Emissions)) %>% ggplot(aes(year, sum_Emi_Vehicles)) + geom_line(colour= "red") + facet_grid(. ~ fips)
