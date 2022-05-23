#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Program: 2.2 LM forest                                                   ##
##  Author:                                                                  ##
##  Purpose: Run an LM forest on the cleaned taxi tips data
##  Last Updated:                                                            ##
##  Notes:                                                                   ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Step 0: Basic set up -----
set.seed(1234)
rm(list = ls())
## Libraries

## Install the packages 

list.of.packages <- c("tidyverse","dplyr","fastDummies","gtsummary","labelled","lmtest","sandwich","grf","glmnet","sandwich","splines","ggplot2","ggpubr","data.table","qwraps2","rpart","MASS","pracma","haven","estimatr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

for(pkg in list.of.packages){
  library(pkg, character.only = TRUE)
}

## File path

user = Sys.info()[["user"]]
if (user == "51989") {
  In_Data <- "C:/Users/51989/OneDrive/Escritorio/Dropbox/Default Tips Project/Data/Intermediate/"
  Output <- "C:/Users/51989/OneDrive/Escritorio/Dropbox/Default Tips Project/Output/"
}

if (user == "brunoei"){
  In_Data <- "/Users/brunoei/Dropbox/Default Tips Project/Data/Intermediate/"
  Output  <- "/Users/brunoei/Dropbox/Default Tips Project/Output/"
}

## Today's date
Today <- format(Sys.Date(), "%d%m%Y")

# Step 1: Import the data -----
tips2009_1218 <- read.csv(paste0(In_Data,"fare_1218_recoded.csv"))
data = subset(tips2009_1218, fare>=14 & fare<=16)

# Step 2: Applied X-learner to the 1020 data -----
n <- nrow(data)
# Treatment: Whether the fare amount is above or below 15 dollars
treatment <- "dsc_15"
# Outcome: Whether someone tips 0. 1 for yes, 0 for no.
outcome <- "tip_zero"
running = "fare"
# Additional covariates
covariates <- c("weekend", "pickup_time_group", "gr_inc10_All", "Manhattan_pkp", "Brooklyn_pkp",
                "Queens_pkp", "Bronx_pkp", "Staten_pkp", "Other_pkp", "Manhattan_drf", 
                "Brooklyn_drf", "Queens_drf", "Bronx_drf", "Staten_drf", "Other_drf")

# Split the data into training and testing
split1<- sample(c(rep(0, 0.7 * nrow(data)), rep(1, 0.3 * nrow(data))))
data.train <- data[split1 == 0,]
data.test <- data[split1 == 1,]
# Training code
W_train <- data.train[,treatment]
Y_train <- data.train[,outcome]
X_train <- data.train[,covariates]
Z1_train = (data.train[,running]-15)*W_train
Z0_train = (data.train[,running]-15)*(1-W_train)
# Test code
W_test <- data.test[,treatment]
Y_test <- data.test[,outcome]
X_test <- data.test[,covariates]
Z1_test = (data.test[,running]-15)*W_test
Z0_test = (data.test[,running]-15)*(1-W_test)

## LM forest
lmf = lm_forest(X_train, Y_train, cbind(W_train, Z0_train, Z1_train))
tau.hat <- predict(lmf,X_test)$predictions[, 1, ]

# Show the CATE distribution 
png(file=paste0(Output, "LM_forest_CATE.png"),width=595, height=545)
hist(tau.hat, main = "Distribution of CATEs, LM forest", xlab = "CATE")
dev.off()

# NOTE: At the moment, the workspace with this data and estimations are stored in Output\lm_forest\lm_f_workspace_bruno