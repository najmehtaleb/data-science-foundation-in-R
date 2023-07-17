
# Read Data 
df <- read.table("exdata_data_household_power_consumption/household_power_consumption.txt", 
                 sep = ";", header = TRUE, na.strings = "?")

# Convert string date to Date
df$Date <- as.Date(df$Date, "%d/%m/%Y")

# Select specific Dates
selected_dates <- df[(df$Date=="2007-02-01" | df$Date=="2007-02-02"), ]

# Create timeline 
selected_dates$Time_line <- strptime(paste(selected_dates$Date, selected_dates$Time), 
                                     "%Y-%m-%d %H:%M:%S")

# Create the plot
with(selected_dates, {
  plot(Time_line, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering") 
  lines(Time_line, Sub_metering_2, col="red")
  lines(Time_line, Sub_metering_3, col="blue")
  })

legend("topright", col = c("black", "red", "blue"), 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
       lty = 1, text.font = 8)

# Save result in a png file
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()