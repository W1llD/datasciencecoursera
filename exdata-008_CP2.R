##plot1
readRDS("summarySCC_PM25.rds") -> NEI
readRDS("Source_Classification_Code.rds") -> scc

library(dplyr)
group_by(NEI,year) -> NEI
summarise(NEI,sum(Emissions)) -> yEmi
plot(yEmi)

