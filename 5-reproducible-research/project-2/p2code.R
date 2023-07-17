
# This database tracks characteristics of major storms and weather events in 
# the United States, including when and where they occur, as well as estimates 
# of any fatalities, injuries, and property damage.
# The events in the database start in the year 1950 and end in November 2011.

url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
file = "repdata_data_StormData.csv.bz2"
if(!file.exists(file)) download.file(url = url, destfile = file)

df  <- read.csv(file)

#subset_df <- df[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

# ------------------------------------------------------------------------------
# 1. Across the United States, which types of events (as indicated in the 
# EVTYPE variable) are most harmful with respect to population health?

pop_df <- df[, c("EVTYPE", "FATALITIES", "INJURIES")]

# There are a lot of 0s.
summary(pop_df)
boxplot(pop_df[, c("FATALITIES", "INJURIES")] )
hist(pop_df$INJURIES)
hist(pop_df$FATALITIES)

# Remove the data points with 0.
pop_df <- pop_df[(pop_df$FATALITIES > 0 | pop_df$INJURIES > 0 ), ]
length(unique(pop_df$EVTYPE))
pop_df$EVTYPE <- as.factor(pop_df$EVTYPE)
plot(pop_df$INJURIES ~ pop_df$EVTYPE)
plot(pop_df$FATALITIES ~ pop_df$EVTYPE)

total_injuries <- aggregate(pop_df$INJURIES ~ pop_df$EVTYPE, FUN = sum)
colnames(total_injuries) <- c("EVTYPE", "INJURIES")
barplot(height = total_injuries$INJURIES)
total_injuries <- total_injuries[order(total_injuries$INJURIES, decreasing = TRUE), ]
barplot(height = total_injuries$INJURIES, ylim = c(0,1000))
total_injuries <- total_injuries[total_injuries$INJURIES > 1000, ]
barplot(height = total_injuries$INJURIES, ylim = c(0,5000))

total_fatalities <- aggregate(pop_df$FATALITIES ~ pop_df$EVTYPE, FUN = sum)
colnames(total_fatalities) <- c("EVTYPE", "FATALITIES")
total_fatalities <- total_fatalities[order(total_fatalities$FATALITIES, decreasing = TRUE), ]
barplot(height = total_fatalities$FATALITIES, ylim = c(0, 1000))
temp <- total_fatalities[total_fatalities$FATALITIES > 60, ]
barplot(temp$FATALITIES[1:10])

# ------------------------------------------------------------------------------

# Across the United States, which types of events have the greatest economic 
# consequences?

eco_df <- df[, c("EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
summary(eco_df)

temp[temp$cropExp == "" | temp$cropExp == "?", c("cropExp")] <- "0"
temp[temp$cropExp == "K" | temp$cropExp == "k", c("cropExp")] <- "1000"
temp[temp$cropExp == "M" | temp$cropExp == "m", c("cropExp")] <- "1000000"
temp[temp$cropExp == "B", c("cropExp")] <- "1000000000"
temp$cropExp <- as.numeric(temp$cropExp)

temp[temp$propExp %in% c("", "-"), c("propExp")] <- 0
temp[temp$propExp %in% c("+"), c("propExp")] <- 1
temp[temp$propExp %in% c("0", "5", "6", "4","2", "7", "3"), c("propExp")] <- 10
temp[temp$propExp %in% c("h", "H"), c("propExp")] <- 100
temp[temp$propExp %in% c("K", "k"), c("propExp")] <- 1000
temp[temp$propExp %in% c("M", "m"), c("propExp")] <- 1000000

temp$cropDamage <- temp$CROPDMG*temp$cropExp
temp$propDamage <- temp$PROPDMG*temp$propExp

crop <- temp[temp$cropDamage > 0, c("EVTYPE", "cropDamage")]
all_crop_damage <- aggregate(crop$cropDamage ~ crop$EVTYPE, FUN = sum)
colnames(all_crop_damage) <- c("EVTYPE", "cropDamage")
all_crop_damage <- all_crop_damage[order(all_crop_damage$cropDamage, decreasing = TRUE), ]
barplot(all_crop_damage$cropDamage[1:10])

prop <- temp[temp$propDamage > 0, c("EVTYPE", "propDamage")]
all_prop_damage <- aggregate(prop$propDamage ~ prop$EVTYPE, FUN = sum)
all_prop_damage <- all_prop_damage[order(all_prop_damage$propDamage, decreasing = TRUE), ]
barplot(all_prop_damage$propDamage[1:10])
