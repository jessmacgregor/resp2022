## 12/6/2022 START HERE. data is in correct order but need to check all notes make sure
#everything lines up

#Script 1 - finding ALL slopes across all sites

## Here I will try to read in all data from summer 2022, combine them, and make site column!! 
install.packages("reshape")
library(here) # Keep file directory/paths clear
library(tidyverse)
library(dplyr)
library(stringr) # Extract info from file names
library(purrr) # Map function across multiple files
library(writexl) # Save new dataframe
library(lubridate) # Select only dates of interest 
library(broom)
library(reshape2)
#load packages

# Return vector of paths to all csv files (using "here" package)
(allsites2022 = list.files(path = here('C:/Users/macgr/Documents/Stony_Brook_Masters/Scallop_project/resp2022/csvfiles'),
                       pattern = ".csv",
                       full.names = TRUE,
                       recursive = TRUE))

(allnames2022 = str_split( allsites2022[2], pattern = "_", simplify = TRUE))

#for loop to read in files and clean them up
siteslist2022 <- list()
i <- 1
allsites_df2022 = NULL
for (path in allsites2022){
  df2022 <- read.csv(path)
  df2022 <- df2022[,1:8]
  colnames(df2022) <- c("time", "DO1", "sal", 
                    "temp", "pressure", "DO2", 
                    "DO3", "DO4") 
  path_split <- str_split( path, pattern = "_", simplify = TRUE)
  df2022$site = path_split[8] #make site column from doc name
  df2022$trial = path_split[9] #make trial column from doc name
  df2022$month = path_split[5]
  if (is.null(allsites_df2022)){
    allsites_df2022 = df2022
  }
  else{
    allsites_df2022 = rbind(allsites_df2022, df2022)
  }
  siteslist2022[i] <- df2022
  i <- i+ 1
}
str(allsites_df2022)

#getting data to long form and creating column with time code to be able to subset later
allsiteslong2022 <- allsites_df2022 %>%
  pivot_longer(c(DO1, DO2, DO3, DO4), 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(time_code =
           case_when(time <= 15 ~ "1_10", 
                     time <= 25 ~ "10_20",
                     time <= 35 ~ "20_30", 
                     time <= 45 ~ "30_40", 
                     time <= 55 ~ "40_50", 
                     time <= 70 ~ "50_60"
           )
  ) %>%
  mutate(month = 
           case_when(month == "06" ~ "earlyJuly",
                     month == "07" ~ "earlyJuly", 
                     month == "08" ~ "August"
                      )
  ) %>%
  mutate(trial = 
           case_when(trial == "Trial1" ~ "T1", 
                     trial == "Trial2" ~ "T2", 
                     trial == "T1" ~ "T1", 
                     trial == "T2" ~ "T2", 
                     trial == "T3" ~ "T3"))

#subsetting only 2 points - 30 seconds after beginning of measure period
#and 30 seconds before end of measure period, these will be used to find slope
subsetallsiteslong2022 <- allsiteslong2022 %>% 
  filter(time %in% c(5.50000, 9.50000, 15.50000, 19.50000, 25.50000, 29.50000,
                     35.50000, 39.50000, 45.50000, 49.50000, 55.50000, 59.50000 ))

#Here lets show what the data actually look like (line plots for each scallop/DO meter and points showing subsetted data)

allplot2022 <- ggplot(allsiteslong2022, aes(time, value, color = month)) + 
  geom_line(color = "steelblue", size = 1) + geom_point(data = subsetallsiteslong2022) + 
  facet_wrap(~site)
allplot2022

#now lets find these slopes - all slopes across all sites. 
fit_model <- function(subsetallsiteslong2022) lm(value ~ time, data = subsetallsiteslong2022)
get_slope <- function(mod) tidy(mod)$estimate[2]

slopes2022 <- subsetallsiteslong2022 %>% 
  group_nest(month, site, trial, variable, time_code) %>% # nest by category
  mutate(model = map(data, fit_model)) %>% # Fit a simple linear model for each category, map applies function to each element of a list. 
  mutate(slope = map_dbl(model, get_slope)) 

#This linear model is the delta in value over the delta in time (in this case 4.0 minutes
#it is not in seconds) It matches when I do this manually. Units are mgO2/L/min 

#NEXT STEPS = 1. choosing usable slopes, 2. adjusting for controls and mass/volume!

# move on to "visualizing_slopes2.R" script
plot(slopes2022$slope)
