# Question 2: 
# Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? 

# Read data
summaryData <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

# Subset Baltimore data
baltData <- summaryData[summaryData$fips=="24510", ]

# Group Emissions by year and take the sum. 
baltTotalEmis <- aggregate(Emissions~year, data = baltData, FUN = sum)

# Make the plot
barplot((Emissions/1000)~year, 
        data=baltTotalEmis, 
        xlab = "Year", 
        ylab = "PM2.5 Emissions (Kiloton)", 
        main="Total PM2.5 Emissions from All Sources in Baltimore City")

# Save the plot in a png file
dev.copy(png, file = "plot2.png")
dev.off()