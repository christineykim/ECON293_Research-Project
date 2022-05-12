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
In_Data <- "/Users/chenyuej/Documents/GitHub/ECON293_Research-Project/Data/Intermediate/"
Raw_Data <- "/Users/chenyuej/Documents/GitHub/ECON293_Research-Project/Data/Raw/"

## Today's date
Today <- format(Sys.Date(), "%d%m%Y")

# Step 1: Cleaning -----

## Read in the stata version of the data
tips2009_clean <- read_dta(paste0(Raw_Data,"tips2009_clean.dta"))

## Subset the data according to the following rules:
## 1. Rides from the vendors only
## 2. Fare between 5 and 25 dollars

tips2009_vendor <- tips2009_clean %>% filter(vendor == 1 & fare > 5 & fare < 25)


tips2009_vendor <- read.csv(paste0(In_Data,"tips2009_vendor.csv"))
write.csv(tips2009_vendor,paste0(In_Data,"tips2009_vendor.csv"))

## Validate the regression results

fml1 <- lm(tip ~ dsc_15 + amt1_15, data = tips2009_vendor)

## This one does not work - R aborted 
lmr_out <- lm_robust(tip ~ dsc_15 + amt1_15, data = tips2009_vendor, fixed_effects = ~ driver_id)

## Save the data 

