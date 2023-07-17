# Question 5:
# How have emissions from motor vehicle sources changed from 1999–2008 
# in Baltimore City?

library(ggplot2)

# Read data
summaryData <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
sourceData <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

# Subset data for Baltimore City
baltData = summaryData[summaryData$fips=="24510", ]

# Subset data for motor vehicle
vehicleData <- sourceData[grepl("vehicle", sourceData$SCC.Level.Two, 
                                ignore.case = TRUE), ]

# Merge data frames
mergedData <- merge(baltData, vehicleData)

# Make the plot
ggplot(mergedData, aes(factor(year), Emissions)) +
  geom_bar(stat = "identity", fill="blue") +
  labs(x="Year", y="PM2.5 Emissions") + 
  labs(title = "PM2.5 Emissions from Motor Vehicle Sources in Baltimore City")


# Save the plot in a png file
dev.copy(png, file = "plot5.png")
dev.off()