setwd("/Users/XW/Desktop/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
#Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for 
#each of the years 1999, 2002, 2005, and 2008
total <- with(NEI, tapply(Emissions, year, sum))
png("plot1.png",width = 480, height = 480)
plot(names(total), total, xlim = c(1998, 2009), xlab = "year", ylab = "total emissions",
     main = "Total Emissions from PM2.5 in US from 1999 to 2008", pch=20)
dev.off()

#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland from 1999 to 2008
setwd("/Users/XW/Desktop/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
Maryland <- subset(NEI,fips=="24510")
subtotal <- with(Maryland, tapply(Emissions, year, sum))
png("plot2.png",width = 480, height = 480)
plot(names(subtotal), subtotal, xlim = c(1998, 2009), xlab = "year", ylab = "total emissions",
     main = "Total Emissions from PM2.5 in Baltimore City from 1999 to 2008", pch=20)
dev.off()

#Of the four types of sources indicated by the type
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City
library(ggplot2)
library(dplyr)
setwd("/Users/XW/Desktop/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
Baltimore <- filter(NEI, fips=="24510")
sub1 <- group_by(Baltimore, year, type)
subtotal3 <- summarize(sub1, total=sum(Emissions))
png("plot3.png", width = 480, height = 480)
ggplot(data =subtotal3, aes(x=year,y=total,colour=type))+geom_line()+ theme(legend.position="right")+ggtitle("Emissions of PM2.5 in Baltimore City from 1999 to 2008 by Types")
dev.off()

#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008
setwd("/Users/XW/Desktop/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
CoalCombustion <- intersect(SCC[grep("*[Cc]ombustion*", SCC$SCC.Level.One), ], SCC[grep("*[Cc]oal*", SCC$SCC.Level.Three),])
sub4 <- subset(NEI, SCC %in% CoalCombustion$SCC)
subtotal4 <- with(sub4, tapply(Emissions, year, sum))
png("plot4.png", width = 480, height = 480)
plot(names(subtotal4), subtotal4, ylab = "emissions", xlab = "year", main = "Emissions from Coal Combustion-related Sources",pch=20)
dev.off()
#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City
setwd("/Users/XW/Desktop/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
MotorVehicle <- intersect(SCC[grep("*[Vv]ehicle*", SCC$SCC.Level.Two), ], SCC[grep("*[Mm]otor*", SCC$SCC.Level.Three),])
sub5 <- subset(NEI, fips=="24510"&NEI$SCC%in%MotorVehicle$SCC)
subtotal5 <- with(sub5, tapply(Emissions, year, sum))
png("plot5.png", width = 480, height = 480)
plot(names(subtotal5), subtotal5, ylab = "emissions", xlab = "year", main = "Emissions from Motor Vehicle in Baltimore City",pch=20)
dev.off()

#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County
setwd("/Users/XW/Desktop/exdata-data-NEI_data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
library(ggplot2)
library(reshape)
MotorVehicle <- intersect(SCC[grep("*[Vv]ehicle*", SCC$SCC.Level.Two), ], SCC[grep("*[Mm]otor*", SCC$SCC.Level.Three),])
BC <- subset(NEI, fips=="24510"&NEI$SCC%in%MotorVehicle$SCC)
BCtotal <- with(BC, tapply(Emissions, year, sum))
LA <- subset(NEI, fips=="06037"&NEI$SCC%in%MotorVehicle$SCC)
LAtotal <- with(LA, tapply(Emissions, year, sum))
df <- data.frame(year = names(BCtotal),BCtotal, LAtotal)
melted = melt(df, id.vars="year")
png("plot6.png", width = 480, height = 480)
ggplot(melted, aes(x=as.numeric(year), y=value, colour=variable))+geom_line()+ggtitle("Emissions from Motor Vehicle in Baltimore and Los Angeles")+xlab("year")
dev.off()