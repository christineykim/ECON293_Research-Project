#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Program: 2.0 X learner                                                   ##
##  Author:                                                                  ##
##  Purpose: Run a x learner on the cleaned taxi tips data
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
Output <- "/Users/chenyuej/Dropbox/Default Tips Project/Output/"

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

# Implement the x learner 
tf0 = regression_forest(X_train[W_train==0,], Y_train[W_train==0])
yhat0 = predict(tf0, X_train[W_train==1,])$predictions
xf1 = regression_forest(X_train[W_train==1,], Y_train[W_train==1]-yhat0)
xf.preds.1 = predict(xf1, X_test)$predictions
xf.preds.1[W_test==1] = predict(xf1,X_test[W_test==1,])$predictions
tf1 = regression_forest(X_train[W_train==1,], Y_train[W_train==1])
yhat1 = predict(tf1, X_train[W_train==0,])$predictions
xf0 = regression_forest(X_train[W_train==0,], yhat1-Y_train[W_train==0])
xf.preds.0 = predict(xf0, X_test)$predictions
xf.preds.0[W_test==0] = predict(xf0,X_test[W_test==0,])$predictions
propf = regression_forest(X_test, W_test)
ehat = predict(propf)$predictions

## Create a causal forest for plotting purposes.
cf.priority = causal_forest(X_train, Y_train, W_train)
priority.cate <- predict(cf.priority, X_test)$predictions

# Show the CATE distribution 
png(file=paste0(Output, "X_Learner_CATE.png"),width=595, height=545)
hist(ehat, main = "Distribution of CATEs, X Learner", xlab = "CATE")
dev.off()