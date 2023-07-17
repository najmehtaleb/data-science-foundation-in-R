# Question 3:
# Of the four types of sources indicated by the 
# type (point, nonpoint, onroad, nonroad) variable, which of these four sources 
# have seen decreases in emissions from 1999–2008 for Baltimore City? Which have 
# seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

library(ggplot2)

# Read data
summaryData <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

# Subset Baltimore data
baltData <- summaryData[summaryData$fips=="24510", ]

# Make the plot
ggplot(baltData, aes(factor(year), Emissions, fill=type)) +
  geom_bar(stat = "identity") + 
  theme_bw() + 
  guides(fill=FALSE) +
  facet_grid(.~type) + 
  labs(x= "Year") + 
  labs(y = "PM2.5 Emissions(Tons)") + 
  labs(title = "PM2.5 Emissions in Baltimore City of Four Types of Sources")

# Save the plot in a png file
dev.copy(png, file = "plot3.png")
dev.off()