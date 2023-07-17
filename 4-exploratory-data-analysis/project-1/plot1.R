
# Read Data 
df <- read.table("exdata_data_household_power_consumption/household_power_consumption.txt", 
                 sep = ";", header = TRUE, na.strings = "?")

# Convert string date to Date
df$Date <- as.Date(df$Date, "%d/%m/%Y")

# Select specific Dates
selected_dates <- df[(df$Date=="2007-02-01" | df$Date=="2007-02-02"), ]

# Plot histogram
hist(selected_dates$Global_active_power, 
     col = "red", main = "Global Active Power", 
     xlab = "Global Active Power (Killowatts)")

# Save result in a png file
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()
