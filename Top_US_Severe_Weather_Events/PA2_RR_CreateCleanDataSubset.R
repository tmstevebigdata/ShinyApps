### DATA PROCESSING

## Initial Setup
rm(list = ls())
setwd('X:/9-Shiny') # Set this to your own directory

options(digits = 2)
options(scipen = 999)

## -- Set variables --
TopNum <- 20

## -- Download source data files --
SrcURL <- 'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
DestFile <- 'repdata-data-StormData.csv.bz2'

if (!file.exists(DestFile)) {
   download.file(url = SrcURL, destfile = DestFile, cacheOK = TRUE)
}

## -- Read Data into a Data Frame and strip of all white spaces --
if (!exists('df_StormData')) {df_StormData <- read.csv(DestFile, header = TRUE, sep = ',', strip.white = TRUE)}

# Add a 'YEAR' column to the Storm Data so that we can analyse the data by Year
df_StormData$YEAR <- as.numeric(format(as.Date(df_StormData[, 'BGN_DATE'], '%m/%d/%Y'), '%Y'))

# Get Median Frequency
StormData_Freq <- as.data.frame(table(df_StormData$YEAR))
names(StormData_Freq)[1] <- c('Year')
Median_Freq <- median(StormData_Freq$Freq)

# Get Median Year
StormData_Freq <- StormData_Freq[StormData_Freq$Freq >= Median_Freq, ]
Median_Year <- as.character(StormData_Freq$Year[1])

# Get all Years where the Year >= Median Year
StormData_Freq <- as.data.frame(table(df_StormData$YEAR))
names(StormData_Freq)[1] <- c('Year')
StormData_Freq <- StormData_Freq[as.character(StormData_Freq$Year) >= Median_Year, ]

## Extract a Subset of the Storm Data for analysis
df_StormData_Subset <- df_StormData[df_StormData$YEAR %in% StormData_Freq$Year, c('YEAR', 'EVTYPE', 'FATALITIES', 'INJURIES', 'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP')]

## Perform data cleaning/conversion

## Define Trim Function
## This function removes leading and trailing spaces from a string
fTrim <- function(x) gsub('^\\s+|\\s+$', '', x)

## Clean up 'EVTYPE' data by removing leading and trailing spaces and converting the text to uppercase
df_StormData_Subset$EVTYPE <- toupper(fTrim(df_StormData_Subset$EVTYPE))

## Clean up 'EVTYPE' by categorising the types of events
fSREVTYPE <- function(SearchText, ReplaceText) {
    df_StormData_Subset$EVTYPE <- ifelse(df_StormData_Subset$EVTYPE == SearchText, ReplaceText, df_StormData_Subset$EVTYPE)
    return(df_StormData_Subset$EVTYPE)
}

## Remove double spaces in 'EVTYPE'
df_StormData_Subset$EVTYPE <- gsub('  ', ' ', df_StormData_Subset$EVTYPE)

## Read the Weather Types for cleaning
source('WeatherTypes.R')

## Convert 'EVTYPE' by factoring it
df_StormData_Subset$EVTYPE <- as.factor(df_StormData_Subset$EVTYPE)

## Convert all 'FATALITIES' data to Numeric
df_StormData_Subset$FATALITIES <- as.numeric(df_StormData_Subset$FATALITIES)

## Convert all 'INJURIES' data to Numeric
df_StormData_Subset$INJURIES <- as.numeric(df_StormData_Subset$INJURIES)

## -- Calculate Property Damage --

## Convert Exponential Symbol ('PROPDMGEXP') to Uppercase
df_StormData_Subset$PROPDMGEXP <- toupper(df_StormData_Subset$PROPDMGEXP)

## Convert 'PROPDMG' to numeric
df_StormData_Subset$PROPDMG <- as.numeric(df_StormData_Subset$PROPDMG)

## Copy the 'PROPDMG' column to a new column called 'PROPERTY_DAMAGE'
df_StormData_Subset$PROPERTY_DAMAGE <- as.numeric(df_StormData_Subset[, c('PROPDMG')])

## Multiply 'PROPDMG' by its Exponential Factor ('PROPDMGEXP') and store the result in 'PROPERTY_DAMAGE'
df_StormData_Subset$PROPERTY_DAMAGE <- ifelse(df_StormData_Subset$PROPDMGEXP == 'H', df_StormData_Subset$PROPDMG * 100, df_StormData_Subset$PROPERTY_DAMAGE)
df_StormData_Subset$PROPERTY_DAMAGE <- ifelse(df_StormData_Subset$PROPDMGEXP == 'K', df_StormData_Subset$PROPDMG * 1000, df_StormData_Subset$PROPERTY_DAMAGE)
df_StormData_Subset$PROPERTY_DAMAGE <- ifelse(df_StormData_Subset$PROPDMGEXP == 'M', df_StormData_Subset$PROPDMG * 1000000, df_StormData_Subset$PROPERTY_DAMAGE)
df_StormData_Subset$PROPERTY_DAMAGE <- ifelse(df_StormData_Subset$PROPDMGEXP == 'B', df_StormData_Subset$PROPDMG * 1000000000, df_StormData_Subset$PROPERTY_DAMAGE)
df_StormData_Subset$PROPERTY_DAMAGE <- ifelse(df_StormData_Subset$PROPDMGEXP > '0' & df_StormData_Subset$PROPDMGEXP <= '9', df_StormData_Subset$PROPDMG * (10 ^ as.numeric(df_StormData_Subset$PROPDMGEXP)), df_StormData_Subset$PROPERTY_DAMAGE)

## -- Calculate Crop Damage --

## Convert Exponential Symbol ('CROPDMGEXP') to Uppercase
df_StormData_Subset$CROPDMGEXP <- toupper(df_StormData_Subset$CROPDMGEXP)

## Convert 'CROPDMG' to numeric
df_StormData_Subset$CROPDMG <- as.numeric(df_StormData_Subset$CROPDMG)

## Copy the 'CROPDMG' column to a new column called 'CROP_DAMAGE'
df_StormData_Subset$CROP_DAMAGE <- as.numeric(df_StormData_Subset[, c('CROPDMG')])

## Multiply 'CROPDMG' by its Exponential Factor ('CROPDMGEXP') and store the result in 'CROP_DAMAGE'
df_StormData_Subset$CROP_DAMAGE <- ifelse(df_StormData_Subset$CROPDMGEXP == 'H', df_StormData_Subset$CROPDMG * 100, df_StormData_Subset$CROP_DAMAGE)
df_StormData_Subset$CROP_DAMAGE <- ifelse(df_StormData_Subset$CROPDMGEXP == 'K', df_StormData_Subset$CROPDMG * 1000, df_StormData_Subset$CROP_DAMAGE)
df_StormData_Subset$CROP_DAMAGE <- ifelse(df_StormData_Subset$CROPDMGEXP == 'M', df_StormData_Subset$CROPDMG * 1000000, df_StormData_Subset$CROP_DAMAGE)
df_StormData_Subset$CROP_DAMAGE <- ifelse(df_StormData_Subset$CROPDMGEXP == 'B', df_StormData_Subset$CROPDMG * 1000000000, df_StormData_Subset$CROP_DAMAGE)
df_StormData_Subset$CROP_DAMAGE <- ifelse(df_StormData_Subset$CROPDMGEXP > '0' & df_StormData_Subset$CROPDMGEXP <= '9', df_StormData_Subset$CROPDMG * (10 ^ as.numeric(df_StormData_Subset$CROPDMGEXP)), df_StormData_Subset$CROP_DAMAGE)

## Remove unwanted columns from the cleaned subset of the Storm Data
DropCols <- c('PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP')
df_StormData_Subset <- df_StormData_Subset[, !(names(df_StormData_Subset) %in% DropCols)]

## Sort data by Year
df_StormData_Subset <- df_StormData_Subset[order(df_StormData_Subset$YEAR), ]

## Sum all Fatalities
df_Fatalities <- aggregate(df_StormData_Subset$FATALITIES, by = list(df_StormData_Subset$YEAR, df_StormData_Subset$EVTYPE), FUN = sum)
names(df_Fatalities) <- c('YEAR', 'EVTYPE', 'FATALITIES')

## Sum all Injuries
df_Injuries <- aggregate(df_StormData_Subset$INJURIES, by = list(df_StormData_Subset$YEAR, df_StormData_Subset$EVTYPE), FUN = sum)
names(df_Injuries) <- c('YEAR', 'EVTYPE', 'INJURIES')

## Sum all Property Damages
df_Property_Damages <- aggregate(df_StormData_Subset$PROPERTY_DAMAGE / 1000000, by = list(df_StormData_Subset$YEAR, df_StormData_Subset$EVTYPE), FUN = sum)
names(df_Property_Damages) <- c('YEAR', 'EVTYPE', 'PROP_DAMAGE_MIL')

## Sum all Crop Damages
df_Crop_Damages <- aggregate(df_StormData_Subset$CROP_DAMAGE / 1000000, by = list(df_StormData_Subset$YEAR, df_StormData_Subset$EVTYPE), FUN = sum)
names(df_Crop_Damages) <- c('YEAR', 'EVTYPE', 'CROP_DAMAGE_MIL')

## Remove original data set as it is no longer required
rm(df_StormData_Subset)

## Combine All data into 1 data frame
df_StormData_Subset2 <- df_Fatalities
df_StormData_Subset2$INJURIES <- ifelse(df_StormData_Subset2$YEAR == df_Injuries$YEAR & df_StormData_Subset2$EVTYPE == df_Injuries$EVTYPE, df_Injuries$INJURIES, 0)
df_StormData_Subset2$PROP_DAMAGE_MIL <- ifelse(df_StormData_Subset2$YEAR == df_Property_Damages$YEAR & df_StormData_Subset2$EVTYPE == df_Property_Damages$EVTYPE, df_Property_Damages$PROP_DAMAGE_MIL, 0)
df_StormData_Subset2$CROP_DAMAGE_MIL <- ifelse(df_StormData_Subset2$YEAR == df_Crop_Damages$YEAR & df_StormData_Subset2$EVTYPE == df_Crop_Damages$EVTYPE, df_Crop_Damages$CROP_DAMAGE_MIL, 0)

## Remove all rows which have no value (Zero)
df_StormData_Subset2 <- df_StormData_Subset2[!(df_StormData_Subset2$FATALITIES == 0 & df_StormData_Subset2$INJURIES == 0 & df_StormData_Subset2$PROP_DAMAGE_MIL == 0 & df_StormData_Subset2$CROP_DAMAGE_MIL == 0), ]

## Sort data by 'EVTYPE' then by Year
df_StormData_Subset2 <- df_StormData_Subset2[order(df_StormData_Subset2$EVTYPE), ]
df_StormData_Subset2 <- df_StormData_Subset2[order(df_StormData_Subset2$YEAR), ]

## -- Top Fatalities --
df_Fatalities <- aggregate(df_StormData_Subset2$FATALITIES, by = list(df_StormData_Subset2$EVTYPE), FUN = sum)
names(df_Fatalities) <- c('EVType', 'Fatalities')
df_Fatalities <- df_Fatalities[order(df_Fatalities$Fatalities, decreasing = TRUE), ]
df_Fatalities <- head(df_Fatalities, TopNum)

## -- Top Injuries --
df_Injuries <- aggregate(df_StormData_Subset2$INJURIES, by = list(df_StormData_Subset2$EVTYPE), FUN = sum)
names(df_Injuries) <- c('EVType', 'Injuries')
df_Injuries <- df_Injuries[order(df_Injuries$Injuries, decreasing = TRUE), ]
df_Injuries <- head(df_Injuries, TopNum)

## -- Top Property Damages --
df_Property_Damages <- aggregate(df_StormData_Subset2$PROP_DAMAGE, by = list(df_StormData_Subset2$EVTYPE), FUN = sum)
names(df_Property_Damages) <- c('EVType', 'Value_Mil')
df_Property_Damages$Value_Mil <- round(df_Property_Damages$Value_Mil, digits = 2)
df_Property_Damages <- df_Property_Damages[order(df_Property_Damages$Value_Mil, decreasing = TRUE), ]
df_Property_Damages <- head(df_Property_Damages, TopNum)

## -- Top Crop Damages --
df_Crop_Damages <- aggregate(df_StormData_Subset2$CROP_DAMAGE, by = list(df_StormData_Subset2$EVTYPE), FUN = sum)
names(df_Crop_Damages) <- c('EVType', 'Value_Mil')
df_Crop_Damages$Value_Mil <- round(df_Crop_Damages$Value_Mil, digits = 2)
df_Crop_Damages <- df_Crop_Damages[order(df_Crop_Damages$Value_Mil, decreasing = TRUE), ]
df_Crop_Damages <- head(df_Crop_Damages, TopNum)

## Save data that can be easily loaded in R
saveRDS(df_Fatalities, 'StormData-Subset-Fatalities.rds')
saveRDS(df_Injuries, 'StormData-Subset-Injuries.rds')
saveRDS(df_Property_Damages, 'StormData-Subset-Property-Damages.rds')
saveRDS(df_Crop_Damages, 'StormData-Subset-Crop-Damages.rds')
