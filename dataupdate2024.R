library(tidyverse)
library(lubridate)

#Will need the above libraries included for later to update these files found in the BCOE folder under ShinyApp_PublishedCode #
#replace the below file path with the one on your computer after downloading the files#

#### Current data sets to work with and add on to ####

#All months data set, contains monthly mean for AQI, & PM2.5. Number of observations, labeled by month, distance and income factors included # 
#ALL_Months_Merged <- read.csv("~Anne/BCOE/AirQualityApp/ALL_Months_Merged_old.csv")
#View(ALL_Months_Merged)

#Seasons data set, contains AQI, pm2.5, locations sub categorized by seasons, closest interstate and distance to it, as well as med_income   #
#ALL_Seasons_Merged <- read.csv("~Anne/BCOE/AirQualityApp/ALL_Seasons_Merged.csv")
#View(ALL_Seasons_Merged)

#All sites merged daily 2_5, contains information like weather, precipitation, distance to something, contains only months_days (for date).. #
#All_Sites_Merged_Daily_2.5 <- read.csv("~Anne/BCOE/AirQualityApp/All_Sites_Merged_Daily_2.5.csv")
#View(All_Sites_Merged_Daily_2.5)

#All years merged, contains AQI~ for why though?, pm2.5, PM 10? means for each year ... #
#ALL_Years_Merged <- read.csv("~Anne/BCOE/AirQualityApp/ALL_Years_Merged.csv")
#View(ALL_Years_Merged)

#Daily PM2.5, contains date, year and PM2.5 value. Date is split multiple ways for some reason... focus on months_days#
#daily_PM2.5 <- read.csv("~Anne/BCOE/AirQualityApp/daily_PM2.5.csv")
#View(daily_PM2.5)
#Used for daily in the dataCleaning.R #
#Used for the Holiday tab #

#New datasets that need to be manipulated so they can be added to current datasets#
`2022_PM2.5_Raw` <- read.csv("~Anne/BCOE/AirQualityApp/2022_PM2.5_Raw.csv")
`2023_PM2.5_Raw` <- read.csv("~Anne/BCOE/AirQualityApp/2023_PM2.5_Raw.csv")
#Used in updating the seasonal tab #
a_2024_PM2.5_Raw <- read.csv("~Anne/BCOE/AirQualityApp/a_2024_PM2.5_Raw.csv")

#View(`2022_PM2.5_Raw`)
#View(`2023_PM2.5_Raw`)
#View(a_2024_PM2.5_Raw)

#Break data set date into 3 subsections month, day, year for each set#  
data_2022 <- `2022_PM2.5_Raw` %>% 
  separate(col = Date, into = c("month","day","year"), sep = "/", remove = FALSE) %>% 
  #Separating date into months_days#
  #  mutate(months_days == "month"+"/"+"day") %>%
  #selecting only used columns#
  select(Date, month, day, year, Daily.Mean.PM2.5.Concentration, DAILY_AQI_VALUE, Site.Name) #include months_days when fixed#

data_2023 <- `2023_PM2.5_Raw` %>% 
  separate(col = Date, into = c("month","day","year"), sep = "/", remove = FALSE) %>% 
  #Separating date into months_days#
  #  mutate(months_days == "month"+"/"+"day") %>%
  #selecting only used columns#
  select(Date, month, day, year, Daily.Mean.PM2.5.Concentration, DAILY_AQI_VALUE, Site.Name) #include months_days when fixed#

data_2024 <- a_2024_PM2.5_Raw %>% 
  separate(col = Date, into = c("month","day","year"), sep = "/", remove = FALSE) %>% 
  #Separating date into months_days#
  #  mutate(months_days == "month"+"/"+"day") %>%
  #selecting only used columns#
  select(Date, month, day, year, Daily.Mean.PM2.5.Concentration, DAILY_AQI_VALUE, Site.Name) #include months_days when fixed#

#Formatting for later code, used and possibly repeated in code#
as.double(data_2022$Daily.Mean.PM2.5.Concentration)
as.double(data_2023$Daily.Mean.PM2.5.Concentration)
# using 2024 as leap year reference
days.year <- seq( as.Date("2024-01-01"), as.Date("2024-12-31"), by="+1 day")
days.year <- data.frame(days.year = days.year) %>% 
  mutate(all_months_days = format(days.year, "%m/%d"))
#merge all the the PM2.5 data sets: (DAILY) #
years <- c(2022, 2023)
# The 2022 and 2023 datasets contain the same 25 unique sites
# Make sure to check this if adding multiple years
unique.sites <- unique(data_2022$Site.Name)

###############################################################
# Completed for updating the All_Sites_Merged_Daily_2.5 file  #
# Assumes you have pre-loaded the new data obtained           #
# from the epa website that needs to be added to the app      #
# also assumes use of the packages included at the start of   #
# this document.                                              #
# Used  AQI_Merging_pm2.5 by: Lillian Haine as an inspiration #
# Completed: July 22nd, 2022, By: Kody DeGolier               #
# Completed: March 5, 2024, By: Anne Hackman                  #
###############################################################
all_sites = matrix(NA, ncol = 3+length(years))

for (j in 1:length(unique.sites)){
  cur.site = unique.sites[j]
  site.dat = cbind(rep(cur.site, nrow(days.year)), days.year$all_months_days, seq(1, 366, 1))
  #If only updating 1 years worth of data remove the for loop for i and the if statements ensuring the data set is in dat.temp #
  for(i in 1:length(years)){
    year = years[i]
    if (i == 1) {dat.temp =data_2022 %>% #2022 data set#
      filter(Site.Name == cur.site) %>% 
      mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>% 
      mutate(month_day = format(date, "%m/%d"),
             day = format(date, "%d"))
    merged.temp = merge(x = dat.temp, y = days.year, 
                        by.x = "month_day", by.y = "all_months_days", all = T) %>% 
      select(month_day, Daily.Mean.PM2.5.Concentration) 
    }
    #### Separated for simplicity with current process #####
    
    if (i==2) {dat.temp = data_2023 %>% #2023 data set
      filter(Site.Name == cur.site) %>% 
      mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>% 
      mutate(month_day = format(date, "%m/%d"), 
             day = format(date, "%d"))
    merged.temp = merge(x = dat.temp, y = days.year, 
                        by.x = "month_day", by.y = "all_months_days", all = T) %>% 
      select(month_day, Daily.Mean.PM2.5.Concentration)
    }
    
    #Takes the mean daily value if repeated measures for that day #
    if (nrow(merged.temp) > 366) {
      noreps.temp = matrix(NA, nrow = 366,ncol = 2)
      unique.dates = unique(merged.temp$month_day)
      for(k in 1:length(unique.dates) ){
        cur.date = unique.dates[k]
        reps = filter(merged.temp, month_day == cur.date)
        
        if(nrow(reps) == 1) {
          val = reps[,2]
        }
        if( (nrow(reps) > 1) ){
          val.vect = reps[,2]
          val = mean(val.vect)
        }
        
        noreps.temp[k, ] =  c(cur.date, val)
      }
      merged.temp = noreps.temp
    }
    temp.measures = merged.temp[,2]
    site.dat = cbind(site.dat, temp.measures)
  } #End of for loop (i) #
  #Names the columns for you based off whats listed in years #
  colnames(site.dat) = c("site.name", "months_days", "day", paste0("PM2.5_", years))
  all_sites = rbind(all_sites, site.dat)
} #End of for loop (j)#

#Combines the information obtained from for loop (j) #
all_sites <- all_sites[-1,]

#Simplified data set turned into a data frame that follows its priors pattern except it is missing static information#
all_sites = data.frame(all_sites)

#Removing duplicated columns#
#Changed day to integer to match variable type in ALL_Sites_Merged_Daily_2.5#
all_sites <- all_sites %>%
  select(site.name, months_days, day, PM2.5_2022, PM2.5_2023) %>%
  mutate(day = as.integer(day)) %>%
  mutate(PM2.5_2022 = as.double(PM2.5_2022)) %>%
  mutate(PM2.5_2023 = as.double(PM2.5_2023))

#9150 observations at this point in all_sites, 12078 observations of 62 variables in All_Sites_Merged ##
#Updated data set #
Updated_ALL_Sites_Merged_Daily_2.5 <- All_Sites_Merged_Daily_2.5 %>%
  #Left joining so we won't need access to the static data or the previous data sets, by = NULL so it matches all possible like columns# 
  left_join(all_sites, by = NULL) #

#Updated dataset now has 12078 observations of 64 variables#
dim(Updated_ALL_Sites_Merged_Daily_2.5)
length(unique.sites)*366

# Checking the number of missing values in updated data set #
# difference in number of rows between the left and right datasets
diff_in_rows = nrow(All_Sites_Merged_Daily_2.5) - nrow(all_sites)
# number of NAs in left dataset
na1 = sum(is.na(All_Sites_Merged_Daily_2.5))
# columns that differ between left and right datasets
addcols = setdiff(names(all_sites),names(All_Sites_Merged_Daily_2.5))
# number of NAs in cols that only appear in right dataset
na2 = sum(is.na(all_sites[addcols]))
#number of NAs in merged dataset
na3 = sum(is.na(Updated_ALL_Sites_Merged_Daily_2.5))
# the multiplication represents that NA will appear in each new column for each row not appearing in right dataset
na3 == na2 + na1 + length(addcols)*diff_in_rows


######################
# Anne H updating 2022/2023 weather for All_Sites_Merged_Daily_2.5
# NOTE: the original ALL_Seasons_Merged dataset included weather for all sites in Hennepin and Ramsey Counties
# Anne H only updated the 2022 and 2023 weather for the five sites on the Weather & AQI tab
# Most of the other Hennepin/Ramsey sites from the original dataset do not appear in the 2022/2023 EPA data
# Completed: March 5, 2024
######################

# Reading in downloaded weather data 2020-2024 from DNR weblink above
dnr_weather_data <- read.csv("~Anne/BCOE/AirQualityApp/downloaded_weather_data.csv")

# Subsetting to just 2022/2023 dates
dnr_weather_data <- dnr_weather_data %>%
  separate(col = Date, into = c("month","day","year"), sep = "/", remove = FALSE) %>%
  filter(year %in% c("22", "23")) #dimensions are as expected: 730 rows x 6 columns

# Correcting format to create months_days variable that matches ALL_Sites_Merged_Daily_2.5
dnr_weather_data <- dnr_weather_data %>%
  mutate(month = ifelse(nchar(month) == 1, paste0("0", month), month)) %>%
  mutate(day = ifelse(nchar(day) == 1, paste0("0", day), day)) %>%
  mutate(months_days = paste0(month, "/", day))

# Splitting into 2022 and 2023 datasets to create weather columns for each year
dnr_weather_data_2022 <- dnr_weather_data %>%
  filter(year == "22")
dnr_weather_data_2023 <- dnr_weather_data %>%
  filter(year == "23")

# Renaming the weather columns to match the format of ALL_Sites_Merged_Daily_2.5
dnr_weather_data_2022 <- dnr_weather_data_2022 %>%
  rename("Maximum.Temperature.degrees..F._2022" = "Maximum.Temperature.degrees..F.",
         "Minimum.Temperature.degrees..F._2022" = "Minimum.Temperature.degrees..F.",
         "Precipitation..inches._2022" = "Precipitation..inches.",
         "Snow..inches._2022" = "Snow..inches.",
         "Snow.Depth..inches._2022" = "Snow.Depth..inches.")

dnr_weather_data_2023 <- dnr_weather_data_2023 %>%
  rename("Maximum.Temperature.degrees..F._2023" = "Maximum.Temperature.degrees..F.",
         "Minimum.Temperature.degrees..F._2023" = "Minimum.Temperature.degrees..F.",
         "Precipitation..inches._2023" = "Precipitation..inches.",
         "Snow..inches._2023" = "Snow..inches.",
         "Snow.Depth..inches._2023" = "Snow.Depth..inches.")

# Joining the two datasets back together and selecting needed columns
new_dnr_weather_data <- dnr_weather_data_2022 %>%
  inner_join(dnr_weather_data_2023, by = "months_days")

new_dnr_weather_data <- new_dnr_weather_data %>%
  select(months_days,
         ends_with("2022"),
         ends_with("2023"))

# Creating dataset with just the five Twin Cities sites to be updated
# Using all_sites dataset created above
# Only need site names and date for left_join below
weather_sites <- all_sites %>%
  filter(site.name %in% c("Andersen School",
                          "Near Road I-35/I-94",
                          "Harding High School",
                          "St. Louis Park City Hall",
                          "Ramsey Health Center"))

weather_sites <- weather_sites %>%
  select(site.name, months_days)

# Attempting to add columns with weather information
# Retains 02/29 as a row of NAs
weather_sites_updated <- weather_sites %>%
  left_join(new_dnr_weather_data, by = "months_days")

# Attempting to add weather info to all_sites
weather_all_sites <- all_sites %>%
  left_join(weather_sites_updated, by = c("site.name", "months_days"))

# Updated dimensions are as expected: 10 weather columns were added
# Updated dataset still has 12708 rows and now has 74 columns
Updated_Weather_ALL_Sites_Merged_Daily_2.5 <- Updated_ALL_Sites_Merged_Daily_2.5 %>%
  left_join(weather_all_sites, by = NULL)

# Checking the number of missing values in updated data set #
# difference in number of rows between the left and right datasets
diff_in_rows = nrow(Updated_ALL_Sites_Merged_Daily_2.5) - nrow(weather_all_sites)
# number of NAs in left dataset
na1 = sum(is.na(Updated_ALL_Sites_Merged_Daily_2.5))
# columns that differ between left and right datasets
addcols = setdiff(names(weather_all_sites),names(Updated_ALL_Sites_Merged_Daily_2.5))
# number of NAs in cols that only appear in right dataset
na2 = sum(is.na(weather_all_sites[addcols]))
#number of NAs in merged dataset
na3 = sum(is.na(Updated_Weather_ALL_Sites_Merged_Daily_2.5))
# the multiplication represents that NA will appear in each new column for each row not appearing in right dataset
na3 == na2 + na1 + length(addcols)*diff_in_rows

# Change the file path to where you'd like to save the new data set to one on your computer, don't forget all '\' should be '/'#
#write.csv(Updated_Weather_ALL_Sites_Merged_Daily_2.5, "~Anne/BCOE/AirQualityApp//New_All_Sites_Merged_Daily_2.5.csv")

#Using this updated data to create updates needed for the other files #
###############################################################

##########################################################
# Section:          New_summarystats                     #
# Completed for updating the ALL_Months_Merged           #
# AND for updating the ALL_Years_Merged                  #
# Gives statistics that require some more manipulation   #
# before merging the files                               #
# Completed: July 22nd, 2022, By: Kody DeGolier          #
# Completed: February 28th, 2024, By: Anne Hackman       #
##########################################################

#Getting key information that will be used in other data sets before merging with All_Sites_Merged_Daily_2.5 @@#
unique.sites <- unique(data_2022$Site.Name)
summarystats = matrix(NA, ncol = 4 )
monthlystats = matrix(NA, ncol = 4 )
monthlymean = matrix(NA, ncol = 4)
Month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

#For loop goes through each site and saves the mean value for the year for that site#
for (l in 1:length(unique.sites)) {
  dat.temp = data_2022 %>%
    filter(Site.Name==unique.sites[l])
  temp.name = unique.sites[l]
  yearavgPM2.5 = mean(dat.temp$Daily.Mean.PM2.5.Concentration)
  
  #For loop goes through each month and saves its mean value for that site #
  for (m in 1:12) {
    dat.temp2 = dat.temp %>%
      filter(month == m)
    
    temp.mean = mean(dat.temp2$Daily.Mean.PM2.5.Concentration)
    #print(temp.mean)
    
    temp.month = month.name[m]
    monthlystats = cbind(temp.name, temp.month, temp.mean, yearavgPM2.5)
    monthlymean = rbind(monthlymean, monthlystats)
  }
  #Names the columns to correspond with those pre-existing in the other files #
  colnames(monthlymean) = c("site.name", "month", "monthly_mean_pm2.5_2022_from_pm2.5", "mean_pm2.5_2022_from2.5")
  summarystats = rbind(summarystats, monthlymean)
}

# Repeating same process for 2023 data: a for loop would be better than repeating the code
unique.sites2 <- unique(data_2023$Site.Name)
summarystats2 = matrix(NA, ncol = 4 )
monthlystats2 = matrix(NA, ncol = 4 )
monthlymean2 = matrix(NA, ncol = 4)
Month2 = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

#For loop goes through each site and saves the mean value for the year for that site#
for (l in 1:length(unique.sites2)) {
  dat.temp = data_2023 %>%
    filter(Site.Name==unique.sites2[l])
  temp.name = unique.sites2[l]
  yearavgPM2.5 = mean(dat.temp$Daily.Mean.PM2.5.Concentration)
  
  #For loop goes through each month and saves its mean value for that site #
  for (m in 1:12) {
    dat.temp2 = dat.temp %>%
      filter(month == m)
    
    temp.mean = mean(dat.temp2$Daily.Mean.PM2.5.Concentration)
    print(temp.mean)
    
    temp.month = month.name[m]
    monthlystats2 = cbind(temp.name, temp.month, temp.mean, yearavgPM2.5)
    monthlymean2 = rbind(monthlymean2, monthlystats2)
  }
  #Names the columns to correspond with those pre-existing in the other files #
  colnames(monthlymean2) = c("site.name", "month", "monthly_mean_pm2.5_2023_from_pm2.5", "mean_pm2.5_2023_from2.5")
  summarystats2 = rbind(summarystats2, monthlymean2)
}

#bind together the results from 2022 and 2023 (same # of rows)
summarystats = cbind(summarystats, summarystats2)

#Removing odd lines of just NA #
Finalsummarystats <- na.omit(summarystats)
#Removing duplicated data #
Finalsummarystats = unique(Finalsummarystats)

#Removing duplicated columns caused by the cbind statement above
#Dimensions are as expected: six columns, 25 unique sites * 12 months (300) rows
Finalsummarystats = Finalsummarystats[, -c(5,6)]

Finalsummarystats = data.frame(Finalsummarystats)

# Changing "NaN" values in 'monthly mean' columns to NA
# Changing PM2.5 columns to correct data type (numeric)
Finalsummarystats <- Finalsummarystats %>%
  mutate(mean_pm2.5_2022_from2.5 = as.double(mean_pm2.5_2022_from2.5)) %>%
  mutate(mean_pm2.5_2023_from2.5 = as.double(mean_pm2.5_2023_from2.5)) %>%
  mutate(monthly_mean_pm2.5_2022_from_pm2.5 = ifelse(monthly_mean_pm2.5_2022_from_pm2.5 == "NaN", NA, monthly_mean_pm2.5_2022_from_pm2.5)) %>%
  mutate(monthly_mean_pm2.5_2023_from_pm2.5 = ifelse(monthly_mean_pm2.5_2023_from_pm2.5 == "NaN", NA, monthly_mean_pm2.5_2023_from_pm2.5)) %>%
  mutate(monthly_mean_pm2.5_2022_from_pm2.5 = as.double(monthly_mean_pm2.5_2022_from_pm2.5)) %>%
  mutate(monthly_mean_pm2.5_2023_from_pm2.5 = as.double(monthly_mean_pm2.5_2023_from_pm2.5))

# Change the file path to where you'd like to save the new data set to one on your computer, don't forget all '\' should be '/'# 
#write.csv(Finalsummarystats, "~Anne/BCOE/AirQualityApp//months_year_summarystats_2022_2023_PM2.5.csv")

###########################################################
# Creates the finalized data set if named properly        #
# Separates each sites yearly mean PM2.5 values           #
# Assumes you used the Finalsummarystats section          #
# Gives the updated ALL_Years_Merged csv file to be used  #
# Completed: July 22nd, 2022, By: Kody DeGolier           #
# Completed: February 28th, 2024, By: Anne Hackman        #
###########################################################
yearstats = matrix(NA, ncol = 3 )
#Names the columns for you based off whats listed in years #
colnames(yearstats) = c("site.name", "mean_pm2.5_2022_from2.5", "mean_pm2.5_2023_from2.5")

#For loop goes through each site to get the previously calculated year avg PM2.5 value#
for (h in 1:length(unique.sites)){
  cur.site = unique.sites[h]
  temp.data = Finalsummarystats %>%
    filter(site.name == cur.site) %>%
    filter(month == "January") %>%
    select(site.name, mean_pm2.5_2022_from2.5, mean_pm2.5_2023_from2.5)
  
  #Names the columns for you based off whats listed in years #
  colnames(temp.data) = c("site.name", "mean_pm2.5_2022_from2.5", "mean_pm2.5_2023_from2.5")
  
  yearstats = rbind(yearstats, temp.data)
}

#Remove first row of NA data #
yearstats = na.omit(yearstats)

#Merging the data sets #
Updated_ALL_Years_Merged <- ALL_Years_Merged %>%
  #Left joining so we won't need access to the static data or the previous data sets, by = NULL so it matches all possible like columns# 
  left_join(yearstats, by = NULL)

# Checking the number of missing values in updated data set #
# difference in number of rows between the left and right datasets
diff_in_rows = nrow(ALL_Years_Merged) - nrow(yearstats)
# columns that differ between left and right datasets
addcols = setdiff(names(yearstats),names(ALL_Years_Merged))
# number of NAs in left dataset
# right dataset has no NAs (manually checked)
na1 = sum(is.na(ALL_Years_Merged))
#number of NAs in merged dataset
na2 = sum(is.na(Updated_ALL_Years_Merged))
# the multiplication represents that NA will appear in each new column for each row not appearing in right dataset
na2 == na1 + length(addcols)*diff_in_rows

# Change the file path to where you'd like to save the new data set to one on your computer, don't forget all '\' should be '/'# 
#write.csv(Updated_ALL_Years_Merged, "~Anne/BCOE/AirQualityApp//New_ALL_Years_Merged.csv")


###########################################################
# Creates the finalized data set if named properly        #
# Separates each sites monthly mean PM2.5 values          #
# Renames columns to match those previously used          #
# Assumes you used the Finalsummarystats section          #
#                                                         #
# Gives the updated ALL_Months_Merged csv file to be used #
# Completed: July 26th, 2022, By: Kody DeGolier           #
# Completed: February 28th, 2024, By: Anne Hackman        #
###########################################################

#Need the below library to rename columns #
library(dplyr)

temp_summarystats <- Finalsummarystats
#Names the columns for you based off whats listed in ALL_Months_Merged #
# Need as many 'month' columns as years of data you are updating
temp_summarystats <- temp_summarystats %>%
  mutate(month2 = month)
temp_summarystats <- rename(temp_summarystats, "site_ID" = "site.name", 
                            "Month_2022_from_pm2.5" = "month",
                            "Month_2023_from_pm2.5" = "month2")
#Need to add in the ID column to avoid massive duplication of data #
temp_summarystats <- temp_summarystats %>% mutate(ID =  paste0(site_ID, "_", Month_2022_from_pm2.5) )

#Selects only the columns that we need to add to the file #
temp_summarystats <- temp_summarystats %>% 
  select("site_ID", "Month_2022_from_pm2.5", "monthly_mean_pm2.5_2022_from_pm2.5", "Month_2023_from_pm2.5", "monthly_mean_pm2.5_2023_from_pm2.5", "ID")

#Merging the data sets #
Updated_ALL_Months_Merged <- ALL_Months_Merged %>%
  #Left joining so we won't need access to the static data or the previous data sets, by = NULL so it matches all possible like columns# 
  left_join(temp_summarystats, by = NULL)

# Checking the number of missing values in updated data set #
# difference in number of rows between the left and right datasets
diff_in_rows = nrow(ALL_Months_Merged) - nrow(temp_summarystats)
# columns that differ between left and right datasets
addcols = setdiff(names(temp_summarystats),names(ALL_Months_Merged))
# number of NAs in right dataset
na1 = sum(is.na(ALL_Months_Merged))
# number of NAs in cols that only appear in right dataset
na2 = sum(is.na(temp_summarystats[addcols]))
#number of NAs in merged dataset
na3 = sum(is.na(Updated_ALL_Months_Merged))
# the multiplication represents that NA will appear in each new column for each row not appearing in right dataset
na3 == na1 + na2 + length(addcols)*diff_in_rows

# Change the file path to where you'd like to save the new data set to one on your computer, don't forget all '\' should be '/'# 
#write.csv(Updated_ALL_Months_Merged, "~Anne/BCOE/AirQualityApp//New_ALL_Months_Merged.csv")

###############################################################
# Created for updating the daily_PM2.5 file                   #
# ~daily_PM2.5 file used in the Holdays tab~                  #
# Assumes you have pre-loaded the data needing to be added    #
# to the application.                                         #
# Assumes use of the packages included at the start of file   #
# For future updates:                                         #
#Will need to change variable = "PM2.5_(year of data updating)#
# Also, need to change year = (year of data being added)      #
# Completed: July 26th, 2022, By: Kody DeGolier               #
# Completed: February 28th, 2024, By: Anne Hackman            #
###############################################################

#Change year to the year of desired update #
year = 2022
#Using all_sites from update of New_All_Sites_Merged_Daily_2.5
data_update_2022 <- all_sites

# Selecting only 2022 data
data_update_2022 <- data_update_2022 %>%
  select(site.name, months_days, day, PM2.5_2022) %>%
  pivot_longer(cols = PM2.5_2022,
               names_to = "variable",
               values_to = "value") %>%
  mutate(value = as.numeric(value))

# Creating necessary columns to match daily_PM2.5
# Object types manipulated to match daily_PM2.5 variables
data_update_2022 <- data_update_2022 %>%
  mutate(months_days_years = paste0(months_days, "/22")) %>%
  mutate(date = as.Date(months_days_years, format = "%m/%d/%y")) %>%
  separate(col = months_days_years, into = c("month","day","year"), sep = "/") %>%
  mutate(year = 2022) %>%
  mutate(day = as.numeric(day)) %>%
  mutate(month = as.numeric(month)) %>%
  mutate(months_days = ifelse(months_days != "02/29", format(date, "%m/%d"), "02/29")) %>%
  mutate(weekday = weekdays(date)) %>%
  mutate(weekdays_ind = ifelse (weekday == "Saturday" | weekday == "Sunday", "weekend", "weekday")) %>%
  mutate(date = as.character(date)) %>%
  select(site.name, months_days, variable, value, year, date, weekday, day, month, weekdays_ind)

#Rearranged data to more closely follow that in the original file#
data_update_2022 <- data_update_2022 %>%
  arrange(site.name, months_days)

#Removing unnecessary row X from base file#
a_daily_PM2.5 <- daily_PM2.5 %>%
  select(site.name, months_days, variable, value, year, date, weekday, day, month, weekdays_ind)
#Now it is ready to be added to base daily_PM2.5 file #
Updated_daily_PM2.5_2022 <- rbind(a_daily_PM2.5, data_update_2022)

################################################################################
#Repeat process for 2023 data
#Change year to the year of desired update #
year = 2023
#Using all_sites from update of New_All_Sites_Merged_Daily_2.5
data_update_2023 <- all_sites

# Selecting only 2023 data
data_update_2023 <- data_update_2023 %>%
  select(site.name, months_days, day, PM2.5_2023) %>%
  pivot_longer(cols = PM2.5_2023,
               names_to = "variable",
               values_to = "value") %>%
  mutate(value = as.numeric(value))

# Creating necessary columns to match daily_PM2.5
# Object types manipulated to match daily_PM2.5 variables
data_update_2023 <- data_update_2023 %>%
  mutate(months_days_years = paste0(months_days, "/23")) %>%
  mutate(date = as.Date(months_days_years, format = "%m/%d/%y")) %>%
  separate(col = months_days_years, into = c("month","day","year"), sep = "/") %>%
  mutate(year = 2023) %>%
  mutate(day = as.numeric(day)) %>%
  mutate(month = as.numeric(month)) %>%
  mutate(months_days = ifelse(months_days != "02/29", format(date, "%m/%d"), "02/29")) %>%
  mutate(weekday = weekdays(date)) %>%
  mutate(weekdays_ind = ifelse (weekday == "Saturday" | weekday == "Sunday", "weekend", "weekday")) %>%
  mutate(date = as.character(date)) %>%
  select(site.name, months_days, variable, value, year, date, weekday, day, month, weekdays_ind)

#Rearranged data to more closely follow that in the original file#
data_update_2023 <- data_update_2023 %>%
  arrange(site.name, months_days)

#Now it is ready to be added to final daily_PM2.5 file #
Updated_daily_PM2.5 <- rbind(Updated_daily_PM2.5_2022, data_update_2023)

# Dimensions of Updated_daily_PM2.5 file are as expected: 99918 rows by 10 variables
# 366 days x 25 sites x 2 years = 18300 rows added to base daily_PM2.5 file

# Change the file path to where you'd like to save the new data set to one on your computer, don't forget all '\' should be '/'#
#write.csv(Updated_daily_PM2.5, "~Anne/BCOE/AirQualityApp//New_daily_PM2.5.csv")

#########################################################################
# For updating ALL_Seasons_Merged                                       #
# Assumes: Usage of the libraries included at the top of this document  #
#                                                                       #
# go based off equinox and solstice dates for the data being added      #
# Will need data for the desired year and until the Spring of the next  #
# year                                                                  #
# Used  SPPS_CombingingAirData.R by: Sarah Samorodnitsky as a reference #
# Completed: August 5th, 2022, By: Kody DeGolier                        #
# Completed: March 4, 2024, By: Anne Hackman                            #
#########################################################################

seasons <- c("Spring", "Summer", "Fall", "Winter")
sites <-unique(data_2022$Site.Name)
n_sites <- length(sites)

# Combining all the data needed to update file.
PM2.5_data_update <- rbind(data_2022, data_2023, data_2024)

# Date variable not working correctly, need to create date with "202x" year format
PM2.5_data_update <- PM2.5_data_update %>% 
  mutate(newDate = paste0(month, "/", day, "/20", year)) %>%
  # Changing the date column to be a "date" object 
  mutate(Date = as.Date(newDate, format = "%m/%d/%Y"))

spring_start <- as.Date("03/20/2022", "%m/%d/%Y") # March Equinox 2022
summer_start <- as.Date("06/21/2022", "%m/%d/%Y") # June Solstice 2022
fall_start <- as.Date("09/22/2022", "%m/%d/%Y") # September Equinox 2022
winter_start <- as.Date("12/21/2022", "%m/%d/%Y") # December Solstice 2022

#Going to need to include information from spring 2022 to early 2024 to add in 2022 and 2023#

for (year in c(2022, 2023)) {
  # Preparing a dataframe to store the data in. 
  current_air_quality_seasonal <- data.frame(Year = c(rep(year, 4*n_sites)), # 4 seasons for each site
                                             Season = c(rep(seasons, each = n_sites)), # for each season, record site reading
                                             Site = rep(sites, 4),
                                             PM2.5 = numeric(4*n_sites),
                                             AQI = numeric(4*n_sites),
                                             N = numeric(4*n_sites)) # number of observations that went into average
  
  # Season dates for the current year (adding a year on after going through the loop once) 
  #2020 is a leap year so 366 days added instead of 365 **  change for future updates to 365#
  current_spring_start <- spring_start + 365*(year - 2022)
  current_summer_start <- summer_start + 365*(year - 2022)
  current_fall_start <- fall_start + 365*(year - 2022)
  current_winter_start <- winter_start + 365*(year - 2022)
  next_spring_start <- current_spring_start + 365 # adding a year because winter extends to the following year
  
  # Break up the current dataset in seasons
  current_Spring <- PM2.5_data_update[PM2.5_data_update$Date >= current_spring_start & 
                                        PM2.5_data_update$Date < current_summer_start,]
  
  current_Summer <- PM2.5_data_update[PM2.5_data_update$Date >= current_summer_start & 
                                        PM2.5_data_update$Date < current_fall_start,]
  
  current_Fall <- PM2.5_data_update[PM2.5_data_update$Date >= current_fall_start & 
                                      PM2.5_data_update$Date < current_winter_start,]
  
  current_Winter <- PM2.5_data_update[PM2.5_data_update$Date >= current_winter_start & 
                                        PM2.5_data_update$Date < next_spring_start,]
  
  # Adding the data to the seasonal data.frame
  for (season in seasons) {
    for (site in sites) {
      # Subsetting the season 
      current_season_data <- get(paste0("current_", season))
      
      # Subsetting the site
      current_season_site_data <- current_season_data[current_season_data$Site.Name %in% site, ]
      
      # Adding the mean PM2.5 concentration for this season and year
      data_to_add <- c(mean(current_season_site_data$Daily.Mean.PM2.5.Concentration),
                       mean(current_season_site_data$DAILY_AQI_VALUE),
                       nrow(current_season_site_data))
      
      # Add the row
      current_air_quality_seasonal[current_air_quality_seasonal$Year == year &
                                     current_air_quality_seasonal$Season == season &
                                     current_air_quality_seasonal$Site %in% site, c("PM2.5", "AQI", "N")] <-
        data_to_add
    }
  }
  #### Going to need to change the file path to one on your computer, this step acts as an in between / save spot ####
  #write.csv(current_air_quality_seasonal, paste0("~Anne/BCOE/AirQualityApp/", "Seasonal_PM2.5_", year, ".csv"), row.names = FALSE)
}

#The data sets created from the above loop #
Seasonal_PM2.5_2022 <- read.csv("~Anne/BCOE/AirQualityApp/Seasonal_PM2.5_2022.csv")
# View(Seasonal_PM2.5_2020)                #Checking if it worked#
Seasonal_PM2.5_2023 <- read.csv("~Anne/BCOE/AirQualityApp/Seasonal_PM2.5_2023.csv")
#View(Seasonal_PM2.5_2021)                 #Checking if it worked#
# Dimensions of both datasets are as expected:
# 25 unique sites*4 seasons = 100 rows, 6 variables
# NOTE: investigated 2023 NA values - lack of data in latter half of 2023 from some sites

#Need to give both data sets an ID column which combines Site and Season separated by a "_", also need to rename the columns#

#Starting with 2022 #
#Names the columns for you based off whats listed in ALL_Months_Merged #
New_Seasonal_PM2.5_2022 <- rename(Seasonal_PM2.5_2022, "site.name" = "Site", "PM2.5_2022" = "PM2.5", "N_2022" = "N", "AQI_2022" = "AQI")
#Need to add in the ID column to avoid massive duplication of data #
New_Seasonal_PM2.5_2022 <- New_Seasonal_PM2.5_2022 %>% mutate(ID =  paste0(site.name, "_", Season) )  %>%
  select(Season, site.name, PM2.5_2022, AQI_2022, N_2022, ID)

#Continuing with 2023 #
#Names the columns for you based off whats listed in ALL_Months_Merged #
New_Seasonal_PM2.5_2023 <- rename(Seasonal_PM2.5_2023, "site.name" = "Site", "PM2.5_2023" = "PM2.5", "N_2023" = "N", "AQI_2023" = "AQI")
#Need to add in the ID column to avoid massive duplication of data #
New_Seasonal_PM2.5_2023 <- New_Seasonal_PM2.5_2023 %>% mutate(ID =  paste0(site.name, "_", Season) )  %>%
  select(Season, site.name, PM2.5_2023, AQI_2023, N_2023, ID)

#Now to add these columns to the existing data set #
#Starting with 2022 seasonal data#
Updated_ALL_Seasons_Merged <- ALL_Seasons_Merged %>%
  #Left joining so we won't need access to the static data or the previous data sets, by = NULL so it matches all possible like columns# 
  left_join(New_Seasonal_PM2.5_2022, by = NULL)

#Now 2023 seasonal data #
Updated_ALL_Seasons_Merged <- Updated_ALL_Seasons_Merged %>%
  #Left joining so we won't need access to the static data or the previous data sets, by = NULL so it matches all possible like columns# 
  left_join(New_Seasonal_PM2.5_2023, by = NULL)

# Change the file path to where you'd like to save the new data set to one on your computer, don't forget all '\' should be '/'# 
#write.csv(Updated_ALL_Seasons_Merged, "~Anne/BCOE/AirQualityApp//New_ALL_Seasons_Merged.csv")
###############################################################

##################################### Updated files ############################################

###################File made for updating others################################
#Used to make ALL_Years_Merged                                                 #
#Used to make ALL_Months_Merged                                                #
#Needs to be further refined to totally match the above data sets              #
#Planning to do so one at a time                                               #
#New_summarystats <- read.csv("~Anne/BCOE/AirQualityApp/months_year_summarystats_2022_2023_PM2.5.csv")
View(New_summarystats)
################################################################################

###################Updated File ################################################
#Used for make_weather_plot in the helper.R tab                                #
#Used for weather_data in the dataCleaning.R                                   #
#Used for make_weather_plot call in app.R                                      #
#New_All_Sites_Merged_Daily_2.5 <- read.csv("~Anne/BCOE/AirQualityApp//All_Sites_Merged_Daily_2.5.csv")
View(New_All_Sites_Merged_Daily_2.5)
#For testing purposes after renaming the old file to "All_Sites_Merged_Daily_2.5_old"  #
#write.csv(New_All_Sites_Merged_Daily_2.5, "~Anne/BCOE/AirQualityApp/All_Sites_Merged_Daily_2.5.csv")
################################################################################

###################Updated File ################################################
#Used for yearly_data in dataCleaning.R                                        #
#yearly_data is used for the Human impact tab                                  #
#New_ALL_Years_Merged <- read.csv("~Anne/BCOE/AirQualityApp//New_ALL_Years_Merged.csv")
View(New_ALL_Years_Merged)
#For testing purposes after renaming the old file to "ALL_Years_Merged_old"  #
#write.csv(New_ALL_Years_Merged, "~Anne/BCOE/AirQualityApp/ALL_Years_Merged.csv")
################################################################################

###################Updated File ################################################
#Used for monthly_data in dataCleaning.R                                       #
#monthly_data is used by monthly_plot in helper.R                              #
#monthly_plot used in app.R under server                                       #
#Has an extra column i ?                                                       #
#New_ALL_Months_Merged <- read.csv("~Anne/BCOE/AirQualityApp//New_ALL_Months_Merged.csv")
View(New_ALL_Months_Merged)
#For testing purposes after renaming the old file to "ALL_Months_Merged_old"  #
#write.csv(New_ALL_Months_Merged, "~Anne/BCOE/AirQualityApp/ALL_Months_Merged.csv")
################################################################################

###################Updated File ################################################
#Used for daily in dataCleaning.R, which is used for the holiday tab           #
#daily function is mentioned in the app.R under the server section             #
#removed column X hopefully it doesn't cause an issue later                    #
#New_daily_PM2.5 <- read.csv("~Anne/BCOE/AirQualityApp//New_daily_PM2.5.csv")
View(New_daily_PM2.5)
#For testing purposes after renaming the old file to "daily_PM2.5._old"        #
#write.csv(New_daily_PM2.5, "~Anne/BCOE/AirQualityApp/daily_PM2.5.csv")
################################################################################

###################Updated File ################################################
#Used for seasonal_data in dataCleaning.R, which is used for the Seasonal tab  #
#seasonal_data_order function used in helper.R section                         #
#seasonal_plot used in the app.R under the server section                      #
#Has an extra X.1 column ?                                                     #
#New_ALL_Seasons_Merged <- read.csv("~Anne/BCOE/AirQualityApp/New_ALL_Seasons_Merged.csv")
View(New_ALL_Seasons_Merged)
#For testing purposes after renaming the old file to "ALL_Seasons_Merged_old"  # 
#write.csv(New_ALL_Seasons_Merged, "~Anne/BCOE/AirQualityApp/ALL_Seasons_Merged.csv")
################################################################################