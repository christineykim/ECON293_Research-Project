#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Program: 1.0 Cleaning                                                    ##
##  Author:                                                                  ##
##  Purpose: Clean the raw .dta data to produce the final version of the data.
##           Reduce the data size for efficiency purposes. 
##  Last Updated:                                                            ##
##  Notes:                                                                   ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Step 0: Basic set up -----

## Libraries

## Install the packages 

## The causalTree package is not in CRAN, the most common R repository.
## To install it, uncomment the next lines as appropriate.
## install.packages("devtools")  # if you don't have this installed yet.
## devtools::install_github('susanathey/causalTree') 
library(causalTree)

list.of.packages <- c("tidyverse","dplyr","fastDummies","gtsummary","labelled","lmtest","sandwich","grf","glmnet","sandwich","splines","ggplot2","ggpubr","data.table","qwraps2","rpart","MASS","pracma","haven","estimatr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

for(pkg in list.of.packages){
  library(pkg, character.only = TRUE)
}

## File path 
In_Data <- "/Users/chenyuej/Dropbox/Default Tips Project/Data/Intermediate/"
Raw_Data <- "/Users/chenyuej/Dropbox/Default Tips Project/Data/Raw/"

## Today's date
Today <- format(Sys.Date(), "%d%m%Y")

# Step 1: Cleaning -----

## Read in the stata version of the data
tips2009_clean <- read_dta(paste0(Raw_Data,"tips2009_clean.dta"))

## Subset the data according to the following rules:
## 1. Rides from the vendors only
## 2. Fare between 1) 10 to 20 dollars 2) 12 to 18 dollars 

tips2009_vendor_1020 <- tips2009_clean %>% filter(vendor == 1 & fare >= 10 & fare <= 20)
tips2009_vendor_1218 <- tips2009_clean %>% filter(vendor == 1 & fare >= 12 & fare <= 18)

# Step 2: Create additional categorical variables for the covariates -----

classify_cov <- function(data){
  ## Categorize day of week 
  data2 <- data %>% 
    mutate(weekend = ifelse(pkp_dow >= 1 & pkp_dow <= 5, 0, 1))
  ## Categorize time of the day 
  data3 <- data2 %>%
    mutate(pickup_time_group = ifelse(pkp_hour >= 6 & pkp_hour <= 12, 1,
                                     ifelse(pkp_hour >= 13 & pkp_hour <= 16, 2, 
                                            ifelse(pkp_hour >= 17 & pkp_hour <= 20, 3, 0))))
  data_fnl <- data3
  return(data_fnl)
}

tips2009_1020_regroup <- classify_cov(tips2009_vendor_1020)
tips2009_1218_regroup <- classify_cov(tips2009_vendor_1218)

# Step 3: Save the data -----
write.csv(tips2009_1020_regroup,paste0(In_Data,"fare_1020_recoded.csv"))
write.csv(tips2009_1218_regroup,paste0(In_Data,"fare_1218_recoded.csv"))