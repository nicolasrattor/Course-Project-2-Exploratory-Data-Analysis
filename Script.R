
#### Download data ####

download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
              destfile = "data/NEI.zip")

unzip(zipfile = "data/NEI.zip", exdir = "data")


### Packages ####

library(tidyverse)


#### Load the data ####

## The zip file contains two files:

#PM2.5 Emissions Data: This file contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. 
#For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year. Here are the first few rows.

NEI<-readRDS("data/summarySCC_PM25.rds")

# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# The type of source (point, non-point, on-road, or non-road)
# The year of emissions recorded



#Source Classification Code Table
#This table provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. 
#The sources are categorized in a few different ways from more general to more specific and you may choose to explore whatever categories you think 
#are most useful. For example, source “10100101” is known as “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.

SCC<-readRDS("data/Source_Classification_Code.rds")


#### Plot 1 ####
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

head(NEI)

NEI %>% group_by(year) %>% summarise(Emissions=sum(Emissions)) %>% 
  ggplot(aes(year,Emissions)) + geom_line() + geom_point() + 
  theme_bw() + 
  labs(title = "Decrease in total PM2.5 emissions in the United States from 1999 to 2008")

ggsave(
  plot = last_plot(),
  filename = "Plots/Plot1.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 15,
  height = 15
)


#### Plot 2 ####
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland  from 1999 to 2008? Use the base plotting system to make a plot answering this question.

mar<-NEI %>% filter(fips == "24510") %>% group_by(year) %>% summarise(Emissions=sum(Emissions,na.rm = TRUE))
head(mar)

png(filename = "Plots/Plot2.png", width = 800, height = 800)

plot(mar$year,mar$Emissions,type="l",main="Decrease in total PM2.5 emissions in the Baltimore City, Maryland from 1999 to 2008")

dev.off()



#### Plot 3 ####
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

head(NEI)

NEI %>% filter(fips == "24510") %>% group_by(year,type) %>% summarise(Emissions=sum(Emissions,na.rm = TRUE)) %>% 
  ggplot(aes(year,Emissions,color=type)) + geom_line() + geom_point() + 
  theme_bw() + 
  labs(title="Decrease and increase in total PM2.5 emissions in the Baltimore City, Maryland from 1999 to 2008. By type variable")

ggsave(
  plot = last_plot(),
  filename = "Plots/Plot3.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 15,
  height = 15
)

#### Plot 4 ####
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

head(NEI)
head(SCC)
a<-SCC[grep("Coal|coal",SCC$Short.Name),1]

NEI %>% filter(SCC %in% a) %>% group_by(year) %>% summarise(Emissions=sum(Emissions,na.rm = TRUE)) %>% 
  ggplot(aes(year,Emissions)) + geom_line() + geom_point() + theme_bw() + 
  labs(title="Emissions from coal combustion-related sources in the United States from 1999 to 2008")

ggsave(
  plot = last_plot(),
  filename = "Plots/Plot4.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 15,
  height = 15
)

#### Plot 5 ####
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

NEI %>% filter(fips == "24510"&type == "ON-ROAD") %>% 
  group_by(year) %>% summarise(Emissions=sum(Emissions,na.rm = TRUE)) %>% 
  ggplot(aes(year,Emissions))+geom_line()+geom_point()+
  theme_bw()+
  labs(title="Motor vehicle sources changed from 1999–2008 in Baltimore City")

ggsave(
  plot = last_plot(),
  filename = "Plots/Plot5.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 15,
  height = 15
)

#### Plot 6 ####
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

NEI %>% filter(fips %in% c("24510","06037") & type == "ON-ROAD") %>% 
  group_by(year,fips) %>% summarise(Emissions=sum(Emissions,na.rm = TRUE)) %>% 
  ggplot(aes(year,Emissions,color=fips))+geom_line()+geom_point()+
  theme_bw()+
  labs(title="Emissions from motor vehicle sources in Baltimore City and Los Angeles, 1998-2008") + 
  theme(legend.position = "bottom") +
  scale_color_manual(values=c("#999999", "#E69F00"), 
                      name="City",
                      labels=c("Los Angeles, California", "Baltimore, Maryland"))

ggsave(
  plot = last_plot(),
  filename = "Plots/Plot6.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 15,
  height = 15
)




























