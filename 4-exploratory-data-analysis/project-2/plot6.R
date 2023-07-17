# Question 6:
# Compare emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, 
# California (fips == "06037"). Which city has seen greater changes over time 
# in motor vehicle emissions?

library(ggplot2)

# Read data
summaryData <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
sourceData <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

# Subset data for motor vehicle
vehicleData <- sourceData[grepl("vehicle", sourceData$SCC.Level.Two, ignore.case = TRUE), ]

# Subset data for Baltimore and Los Angeles
cityData = summaryData[(summaryData$fips=="24510" | summaryData$fips=="06037" ), ]

cityData$city <- ifelse(cityData$fips=="06037", "Los Angeles", "Baltimore City")

# Merge data frames
mergedData <- merge(vehicleData, cityData)

# Make the plot 
ggplot(mergedData, aes(factor(year), Emissions , fill=city)) +
  geom_bar(stat = "identity", aes(fill=year)) +
  theme_bw() + 
  guides(fill=FALSE) +
  facet_grid(.~city) +
  labs(x="Year", y="PM2.5 Emissions") + 
  labs(title = "PM2.5 Emissions from Motor Vehicle Sources in Baltimore City and Los Angeles")

# Save the plot in a png file
dev.copy(png, file = "plot6.png")
dev.off()