#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Program: 2.1 causal forest learner                                                   ##
##  Author:                                                                  ##
##  Purpose: Run a causal forest on the cleaned taxi tips data
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
library(magrittr)

setwd("/Users/christinekim/Desktop/Courses/ECON 293/project")

## Change user file path 
User <- "/Users/christinekim/"

In_Data <- paste0(User,"Dropbox/Default Tips Project/Data/Intermediate/")
Output <-  paste0(User,"Dropbox/Default Tips Project/Output/")

## Today's date
Today <- format(Sys.Date(), "%d%m%Y")

# Step 1: Import the data -----
tips2009_1218 <- read.csv(paste0(In_Data,"fare_1218_recoded.csv"))
data = subset(tips2009_1218, fare>=14 & fare<=16)

# Step 2: Applied causal forest to the 1416 data -----
n <- nrow(data)
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
## Estimate the causal forest on the test data
cf.eval <- causal_forest(X_test, Y_test, W_test)

## Estimate the ATE for the whole population 
cf.ATE.mean <- mean(cf_predict$predictions)
cf.ATE.SD <- sd(cf_predict$predictions)
ATE_stats <- c(cf.ATE.mean,cf.ATE.SD)
write.csv(ATE_stats,paste0(Output, "Causal_Forest_sumstats.csv"),row.names=F)

# Show the CATE distribution 
#png(file=paste0(Output, "Causal_Forest_CATE.png"),width=595, height=545)
hist(cf_predict$predictions, main = "Distribution of CATEs, Causal Forest", xlab = "CATE", xlim=c(-0.06,0.06))
#dev.off()

## Plot QINI curve - using rank_average_treatment_effect
rate <- rank_average_treatment_effect(cf.eval, cf_predict, target = "QINI")
#png(file=paste0(Output, "Causal_Forest_QINI.png"),width=595, height=545)
plot(rate)
print(rate)
#dev.off()

## Create quartile plots
tau.hat <- cf_predict$predictions
strata <- 10
quintiles <- quantile(tau.hat, prob=seq(from=0,to=1,by=1/strata))
tau.hat.quartiles <- cut(tau.hat, breaks = quintiles, labels = 1:strata, include.lowest = TRUE)
## Combine all the data together
combined_data <- as.data.frame(cbind(data.test,tau.hat,tau.hat.quartiles))
## Store tau estimates by quartiles 
CATE_test <- combined_data %>% group_by(tau.hat.quartiles) %>%
  summarise(tau.mean = mean(tau.hat,na.rm = TRUE),
            tau.sd = sd(tau.hat,na.rm = TRUE))
## Estimate ATE for each split 
ATE.df <- data.frame(ATE=double(),
                     SE=integer())

## Create the calibration plot 
for (i in 1:strata){
  temp_data <- combined_data %>% filter(tau.hat.quartiles == i)
  rd_lm <- temp_data %$%
    lm(tip_zero ~ dsc_15 + I(fare - 15) + dsc_15:I(fare - 15))
  ATE.df[i,"ATE"] <- rd_lm$coefficients["dsc_15"]
  ATE.df[i,"SE"] <- summary(rd_lm)$coefficients[2,2]
}

## CI
ATE.df2 <- ATE.df %>% mutate(CI_max = ATE + 1.96*SE, CI_min = ATE - 1.96*SE)
## Combine the dataset
ATE.df.fnl <- cbind(ATE.df2,CATE_test)

cali_plot <- ggplot(ATE.df.fnl, aes(tau.mean, ATE)) +        # ggplot2 plot with confidence intervals
  geom_point() + 
  geom_errorbar(aes(ymin = CI_max, ymax = CI_min)) + 
  theme_light() + labs(x = "Tau Hat", y = "ATE", title = "Calibration Plot") 

png(file=paste0(Output, "Calibration_by_quartile_Causal_Forest.png"),width=595, height=545)
cali_plot
dev.off()
