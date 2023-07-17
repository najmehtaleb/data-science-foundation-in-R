# Question 1: 
# Have total emissions from PM2.5 decreased in the United States from 
# 1999 to 2008? Using the base plotting system, make a plot showing the 
# total PM2.5 emission from all sources for each of the years 
# 1999, 2002, 2005, and 2008.

# Read the data
summaryData <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

# Group Emissions by year and take the sum. 
totalEmissions <- aggregate(Emissions~year, data = summaryData, FUN = sum)

# Create the plots 
barplot((Emissions/1000)~year, 
        data=totalEmissions, 
        xlab = "Year", 
        ylab = "PM2.5 Emissions (Kiloton)", 
        main="Total PM2.5 Emissions from All Sources in US")

# Save the plot in a png file
dev.copy(png, file = "plot1.png")
dev.off()