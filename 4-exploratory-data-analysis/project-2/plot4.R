# Question 4:
# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?

library(ggplot2)

# Read data
summaryData <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
sourceData <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

# Subset coal combustion-related data 
combData <- sourceData[grep("comb", sourceData$SCC.Level.One, ignore.case = TRUE), ]
coalData <- combData[grep("coal", combData$SCC.Level.Four, ignore.case = TRUE), ]

mergedData <- merge(summaryData, coalData)

ggplot(mergedData, aes(factor(year), Emissions/1000)) +
  geom_bar(stat = "identity") +
  labs(x="Year", y="PM2.5 Emissions (Kiloton)") + 
  labs(title = "PM2.5 Emissions from Coal Combustion-related Sources in US")
  
# Save the plot in a png file
dev.copy(png, file = "plot4.png")
dev.off()