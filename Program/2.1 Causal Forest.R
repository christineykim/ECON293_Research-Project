#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Program: 2.0 X learner                                                   ##
##  Author:                                                                  ##
##  Purpose: Run a x learner on the cleaned taxi tips data
##  Last Updated:                                                            ##
##  Notes:                                                                   ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Step 0: Basic set up -----

## Libraries
remove(list=ls())
library(tidyverse)
library(data.table)
library(Matrix)
library(ggplot2)
library(ggfortify)
library(haven)  
library(grf)
library(glmnet)
library(Matching)
library(dplyr)
library(MASS)

setwd("/Users/christinekim/Desktop/Courses/ECON 293/project")

## Change user file path 
User <- "/Users/christinekim/"

In_Data <- paste0(User,"Dropbox/Default Tips Project/Data/Intermediate/")
Output <-  paste0(User,"Dropbox/Default Tips Project/Output/")

## Today's date
Today <- format(Sys.Date(), "%d%m%Y")

# Step 1: Import the data -----
tips2009_1020 <- read.csv(paste0(In_Data,"fare_1020_recoded.csv"))
tips2009_1218 <- read.csv(paste0(In_Data,"fare_1218_recoded.csv"))

# Step 2: Applied X-learner to the 1020 data -----
data <- tips2009_1020
n <- nrow(tips2009_1020)
# Treatment: Whether the fare amount is above or below 15 dollars
treatment <- "dsc_15"
# Outcome: Whether someone tips 0. 1 for yes, 0 for no.
outcome <- "tip_zero"
# Additional covariates
covariates <- c("weekend", "pickup_time_group", "gr_inc10_All", "Manhattan_pkp", "Brooklyn_pkp",
                "Queens_pkp", "Bronx_pkp", "Staten_pkp", "Other_pkp", "Manhattan_drf", 
                "Brooklyn_drf", "Queens_drf", "Bronx_drf", "Staten_drf", "Other_drf")

# Split the data into training and testing
split1<- sample(c(rep(0, 0.7 * nrow(data)), rep(1, 0.3 * nrow(data))))
data.train <- data[split1 == 0,]
data.test <- data[split1 == 1,]

data.train <- data[split1 == 0,]
data.test <- data[split1 == 1,]

W_train <- data.train[,treatment]
Y_train <- data.train[,outcome]
X_train <- data.train[,covariates]

W_test <- data.test[,treatment]
Y_test <- data.test[,outcome]
X_test <- data.test[,covariates]

# Implement causal forest in grf
cf_model = causal_forest(
  X = X_train,
  Y = Y_train,
  W = W_train
)

# Estimate treatment effects on test data
cf_predict = predict(cf_model, X_test)

# Show the CATE distribution 
png(file=paste0(Output, "Causal_Forest_CATE.png"),width=595, height=545)
hist(cf_predict$predictions, main = "Distribution of CATEs, X Learner", xlab = "CATE")
dev.off()
