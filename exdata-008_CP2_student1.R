library(ggplot2)
library(ggthemes)
library(dplyr)

# Author: Timothy Long
# Date: 11/21/2014
# Years for this assignment: 1999, 2002, 2005, 2008

#Data Dictionary for summarySCC_PM25.rds
#fips: A five-digit number (represented as a string) indicating the U.S. county
#SCC: The name of the source as indicated by a digit string (see source code classification table)
#Pollutant: A string indicating the pollutant
#Emissions: Amount of PM2.5 emitted, in tons
#type: The type of source (point, non-point, on-road, or non-road)
#year: The year of emissions recorded
pm <- readRDS('summarySCC_PM25.rds')

# Source_Classification_Code.rds: This table provides a mapping from the SCC
# digit strings in the Emissions table to the actual name of the PM2.5 source. The 
# sources are categorized in a few different ways from more general to more specific and you
# may choose to explore whatever categories you think are most useful. For example, 
# source “10100101” is known as “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.
codes <- readRDS('Source_Classification_Code.rds')

View(pm)
View(codes)
all <- merge(pm, codes, by='SCC')
View(all)


#1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

summary(as.factor(all$year))

pm25_by_year <- summarise(group_by(all, year), sum(Emissions))
names(pm25_by_year) <- c('Year','Emissions')

png(filename="1.png")
barplot(pm25_by_year$Emissions, 
        main='US annual PM25 emissions are decreasing over time.',  
        names.arg=c('1999','2002','2005','2008'),
        ylab='PM25 Emissions (Tons)'
        )
dev.off()

#2. Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.
balt <- all[all$fips=="24510",]
balt_pm25_by_year <- summarise(group_by(balt, year), sum(Emissions))
names(balt_pm25_by_year) <- c('Year','Emissions')


png(filename="2.png")
barplot(balt_pm25_by_year$Emissions, 
        main='PM25 Emissions are declining for Baltimore City',
        names.arg=c('1999','2002','2005','2008'),
        ylab='PM25 Emissions (Tons)'
)
dev.off()

#3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a 
# plot answer this question.
balt$type <- as.factor(balt$type)
balt_types <- data.frame(summarise(group_by(balt, year, type), sum(Emissions)))
names(balt_types) <- c('Year','Type','Emissions')

png(file='3.png',width=950)
p <- ggplot(balt_types, aes(x=Year,  y=Emissions, group=Type))
p <- p + ggtitle('Non-Point emissions sources sees the largest decline in Baltimore City from 1999-2008.')
p <- p + geom_line(size=2, aes(colour=Type, group=Type))
p <- p + theme_gdocs()
p <- p + scale_y_continuous(limits=c(0, 2200), ylab('PM25 Emissions (Tons)'))
p <- p + scale_x_continuous(limits=c(1998,2010), breaks=seq(1998,2010,2))
p <- p + geom_text(data=balt_types[balt_types$Year==1999 & balt_types$Type!='POINT',], 
                   aes(fontface="bold", colour=Type, label=trunc(Emissions), hjust=1, vjust=0))
p <- p + geom_text(data=balt_types[balt_types$Year==1999 & balt_types$Type=='POINT',], 
                   aes(fontface="bold", colour=Type, label=trunc(Emissions), hjust=1, vjust=1.75))
p <- p + geom_text(data=balt_types[balt_types$Year==2008 & balt_types$Type!='NON-ROAD',], 
                   aes(fontface="bold", colour=Type, label=paste(Type, ": ", trunc(Emissions), sep=""), hjust=-0.1, vjust=-0.25))
p <- p + geom_text(data=balt_types[balt_types$Year==2008 & balt_types$Type=='NON-ROAD',], 
                   aes(fontface="bold", colour=Type, label=paste(Type, ": ", trunc(Emissions), sep=""), hjust=-0.1, vjust=1.25))
p <- p + theme(legend.position="none")
p
dev.off()

#4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
coals <- grepl('Coal', all$EI.Sector, ignore.case=T)
coal <- all[coals,]
coal_by_year <- summarise(group_by(coal, year), sum(Emissions))
names(coal_by_year) <- c('Year','Emissions')
png(file='4.png', width=750)
p <- ggplot(coal_by_year, aes(x=Year, y=Emissions))
p <- p + ggtitle('US annual coal combustion emissions (in tons) are falling from 1999 to 2008.')
p <- p + geom_line(size=2, aes(colour='#333333'), show_guide=F)
p <- p + scale_y_continuous(limits=c(300000,650000), ylab('PM25 Emissions (Tons)'))
p <- p + scale_x_continuous(limits=c(1999,2008))
p <- p + theme_gdocs()
p
dev.off()

#5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
head(balt)
levels(balt$EI.Sector)
mv <- balt[grepl('On-Road',balt$EI.Sector, ignore.case=T),]
mv_by_year <- summarise(group_by(mv, year), sum(Emissions))
names(mv_by_year) <- c('Year','Emissions')

png(file='5.png', width=800)
p <- ggplot(mv_by_year, aes(x=Year, y=Emissions))
p <- p + ggtitle('Baltimore City annual motor vehicle emissions have fallen from 1999 to 2008.')
p <- p + geom_line(size=2, aes(colour='#333333'), show_guide=F)
p <- p + scale_y_continuous(limits=c(0,500), ylab('PM25 Emissions (Tons)'))
p <- p + scale_x_continuous(limits=c(1999,2008))
p <- p + theme_gdocs()
p
dev.off()

#6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
la <-  all[all$fips=="06037",]
la$location<-as.factor('Los Angeles County, California')
levels(la$location)<-c('Los Angeles County, California', 'Baltimore City, Maryland')
summary(la$location)

balt$location <- as.factor('Baltimore City, Maryland')
levels(balt$location)<-c( 'Baltimore City, Maryland','Los Angeles County, California')
summary(balt$location)

comb <- rbind(balt, la)
comb_mv <-  comb[grepl('On-Road',comb$EI.Sector, ignore.case=T),]
comb_mv_by_year <- data.frame(summarise(group_by(comb_mv, year, location), sum(Emissions)))
names(comb_mv_by_year) <- c('Year', 'Location','Emissions')

png(file='6.png', width=975)
p <- ggplot(comb_mv_by_year, aes(x=Year, y=Emissions, group=Location))
p <- p + ggtitle('Baltimore shows slow decline in annual motor vehicle emissions while LA County shows overall rise.')
p <- p + geom_line(size=2, aes(colour=Location, group=Location), show_guide=FALSE)
p <- p + scale_y_continuous(limits=c(0,9000), ylab('PM25 Emissions (Tons)'))
p <- p + scale_x_continuous(limits=c(1999,2008))
p <- p + theme_gdocs()
p <- p + theme(legend.position="none")
p <- p + geom_text(data = comb_mv_by_year[comb_mv_by_year$Year==2008 & comb_mv_by_year$Location=='Baltimore City, Maryland',], aes(fontface="bold", colour=Location, label='Baltimore City, Maryland', sep=""), hjust=1, vjust=-1)
p <- p + geom_text(data = comb_mv_by_year[comb_mv_by_year$Year==2008 & comb_mv_by_year$Location=='Los Angeles County, California',], aes(fontface="bold", colour=Location, label='Los Angeles County, California', sep=""), hjust=1, vjust=-5)
p
dev.off()

