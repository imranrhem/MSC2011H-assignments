# MSC2011H - ASSIGNMENT 4
# IMRAN RHEMTULLA
# JUNE , 2022

library(lubridate)
# Read the dataset as a dataframe

ufo <- read.csv("ufo_subset.csv", header = TRUE, sep = ",")
ufo
summary(ufo)

# Remove spaces from columns that may be present
make.names(names(ufo), unique = TRUE)

# Remove rows that do not have country or space information
ufo$country[ufo$country == ""] <- NA
ufo$shape[ufo$shape == ""] <- NA
ufo <- ufo[!(is.na(ufo$country)),]
ufo <- ufo[!(is.na(ufo$shape)),]

# Conversion of datetime into appropriate format (into POSIX..)
ufo$datetime <- ymd_hms(ufo$datetime)
ufo$date.posted <- ymd(ufo$date.posted)
class(ufo$datetime)
class(ufo$date.posted)
# HOAX REMOVAL 1 - ANY MENTION OF HOAX
sum(grepl("hoax", ufo$comments, ignore.case = TRUE))

#HOAX REMOVAL 2 - ANY MENTION OF hoax + NUFORC note
sum(grepl("NUFORC\\sNote\\:", ufo$comments, ignore.case = FALSE) &
      grepl("hoax", ufo$comments, ignore.case = TRUE))

# Create report_delay column
ufo$report_delay <- as.numeric(difftime(ufo$date.posted, ufo$datetime, units = "days"))
                                     
# Round to 0 for exact days
ufo$report_delay <- round(ufo$report_delay,0)

# Filter out rows where sighting was reported before it occurred, ie. less than 0 days
ufo <- ufo[ufo$report_delay > 0,]

####HERE IS WHERE SHE SAID TO WAIT I BELIEVE ######


# Find average report delay per country

countries <-  as.factor(ufo$country)
country_levels <- levels(countries)

# Create a table

aggregate(ufo$report_delay, list(ufo$country), mean)

