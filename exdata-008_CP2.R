##init
readRDS("summarySCC_PM25.rds") -> NEI
readRDS("Source_Classification_Code.rds") -> scc

library(dplyr)
NEI %>% group_by(year) -> NEI

##plot1
NEI %>% summarise(sum(Emissions)) %>% plot(col="red",type="l")

##plot2
NEI %>% filter(fips == 24510) %>% summarise(sum(Emissions)) %>% plot(col="red",type="l")


