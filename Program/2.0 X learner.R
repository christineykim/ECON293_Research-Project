#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Program: 2.0 X learner                                                   ##
##  Author:                                                                  ##
##  Purpose: Run a x learner on the cleaned taxi tips data
##  Last Updated:                                                            ##
##  Notes:                                                                   ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list=ls())
set.seed(1234)
# Step 0: Basic set up -----

## Libraries

## Install the packages 

## The causalTree package is not in CRAN, the most common R repository.
## To install it, uncomment the next lines as appropriate.
## install.packages("devtools")  # if you don't have this installed yet.
## devtools::install_github('susanathey/causalTree') 
library(causalTree)

list.of.packages <- c("tidyverse","dplyr","fastDummies","gtsummary","labelled","lmtest","plotrix","sandwich","grf","glmnet","sandwich","splines","ggplot2","ggpubr","data.table","qwraps2","rpart","MASS","pracma","haven","estimatr","magrittr")
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
# tips2009_1020 <- read.csv(paste0(In_Data,"fare_1020_recoded.csv"))
tips2009_1217 <- read.csv(paste0(In_Data,"fare_1217_recoded.csv"))

# Step 1.5: Subset the data to 1415 ----
## This step may be deleted once the codes are finalized. The purpose of reducing the dataset 
## is to reduce the running time. 

tips2009_1415 <- tips2009_1217 %>% filter(fare > 14 & fare < 16)

# Step 2: Applied X-learner to the 1217 data -----
data <- tips2009_1415
n <- nrow(tips2009_1415)
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

W_train <- data.train[,treatment]
Y_train <- data.train[,outcome]
X_train <- data.train[,covariates]

W_test <- data.test[,treatment]
Y_test <- data.test[,outcome]
X_test <- data.test[,covariates]

# Implement the x learner on the testing data 
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
preds.xf = (1 - ehat) * xf.preds.1 + ehat * xf.preds.0

# Step 3: Generate the resulted statistics -----

## Estimate the ATE for the whole population 
xf.ATE.mean <- mean(preds.xf)
xf.ATE.SD <- sd(preds.xf)
ATE_stats <- c(xf.ATE.mean,xf.ATE.SD)
write.csv(ATE_stats,paste0(Output, "X_learner_sumstats.csv"),row.names=F)

# Step 4: Generate the diagnostic plots -----

## Create a causal forest for plotting purposes.
cf.priority = causal_forest(X_train, Y_train, W_train)
priority.cate <- predict(cf.priority, X_test)$predictions
## Estimate the causal forest on the test data
cf.eval <- causal_forest(X_test, Y_test, W_test)

## Plot QINI curve - using rank_average_treatment_effect
rate <- rank_average_treatment_effect(cf.eval, preds.xf, target = "QINI")
png(file=paste0(Output, "X_Learner_QINI.png"),width=595, height=545)
plot(rate)
print(rate)
dev.off()

# Show the CATE distribution 
png(file=paste0(Output, "X_Learner_CATE.png"),width=595, height=545)
hist(ehat, main = "", xlab = "CATE")
dev.off()

## Create quartile plots
tau.hat <- preds.xf
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
  theme_light() + labs(x = "Tau Hat", y = "ATE") + 
  theme(axis.text.x = element_text(face="bold",
                                   size=12),
        axis.text.y = element_text(face="bold", 
                                   size=12),
        axis.title=element_text(size=16,face="bold"))

ggsave(file=paste0(Output, "Calibration_by_quartile.png"), plot = cali_plot)

# Step 5: Create HTE plots by weekend -----

HTE_plot <- function(tau.hat,data.test,factor, factor_label){
## Combine all the data together
combined_data <- as.data.frame(cbind(data.test,tau.hat))
## Store tau estimates by drop off borough
CATE_test <- combined_data %>% group_by(get(factor)) %>%
  summarise(tau.mean = mean(tau.hat,na.rm = TRUE),
            tau.sd = sd(tau.hat,na.rm = TRUE))
## Estimate ATE for each split 
ATE.df <- data.frame(ATE=double(),
                     SE=integer(),
                     strata = integer(),
                     method = c())
## Estimate Causal Forest for each split 
CausalF.df <- data.frame(ATE=double(),
                     SE=integer(),
                     strata = integer(),
                     method = c())
## Estimate X learner for each split 
Xlearner.df <- data.frame(ATE=double(),
                         SE=integer(),
                         strata = integer(),
                         method = c())

## Create the calibration plot 
column_of_interest <- combined_data[,colnames(combined_data) == factor]
strata <- length(unique(column_of_interest))

## Store tau hats from each method for plotting purposes
for (i in 1:strata){
  temp_data <- combined_data %>% filter(get(factor) == sort(unique(column_of_interest))[i])
  ## RD
  rd_lm <- temp_data %$%
    lm(tip_zero ~ dsc_15 + I(fare - 15) + dsc_15:I(fare - 15))
  ATE.df[i,"ATE"] <- rd_lm$coefficients["dsc_15"]
  ATE.df[i,"SE"] <- coeftest(rd_lm, vcov=vcovHC(rd_lm, "HC2"))[2,2]
  ATE.df[i,"strata"] <- i 
  ATE.df[i,"method"] <- "Regression Discontinuity"
  ## Causal Forest
  forest <- causal_forest(X=temp_data[,covariates],W=temp_data[,treatment],Y=temp_data[,outcome],num.trees = 100)
  forest.ate <- average_treatment_effect(forest)
  CausalF.df[i,"ATE"] <- forest.ate[1]
  CausalF.df[i,"SE"] <- forest.ate[2]
  CausalF.df[i,"strata"] <- i 
  CausalF.df[i,"method"] <- "Causal Forest"
  ## X learner
  Xlearner.df[i,"ATE"] <- mean(tau.hat)
  Xlearner.df[i,"SE"] <- std_err(tau.hat)
  Xlearner.df[i,"strata"] <- i
  Xlearner.df[i,"method"] <- "X Learner"
}

## Combine the tau hats from all methods 
res <- rbind(ATE.df, CausalF.df)

## Recode values in strata
for (i in 1:strata){
  res$strata[res$strata == i] <- factor_label[i]
}

calibration_plot <- ggplot(res) +
  aes(x = strata, y = ATE, group=method, color=method) + 
  geom_point(position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=ATE-2*SE , ymax=ATE+2*SE), width=.2, position=position_dodge(0.2)) +
  ylab("") + xlab("") +
  ggtitle("Average CATE within each ranking (as defined by predicted CATE)") +
  theme_light() +
  theme(legend.position="bottom", legend.title = element_blank())

return(calibration_plot)
}

## Generate plots 
sort(unique(data.test.drfboro$weekend))
plot_weekend <- HTE_plot(tau.hat,data.test,factor = "weekend", factor_label = c("Weekday", "Weekend"))
ggsave(file=paste0(Output, "HTE_by_weekend.png"), plot = plot_weekend)

sort(unique(data.test.drfboro$pickup_time_group))
plot_TimeofDay <- HTE_plot(tau.hat,data.test,factor = "pickup_time_group", factor_label = c('Morning', 'Afternoon', 'Night'))
ggsave(file=paste0(Output, "HTE_by_timeofday.png"), plot = plot_TimeofDay)

data.test.noNA <- data.test %>% filter(!is.na(gr_inc10_All))
tau.hat2 <- tau.hat[!is.na(data.test$gr_inc10_All)]

sort(unique(data.test.drfboro$gr_inc10_All))
plot_Income <- HTE_plot(tau.hat2,data.test.noNA,factor = "gr_inc10_All", factor_label = c("1","2","3","4","5","6","7","8","9","10"))
ggsave(file=paste0(Output, "HTE_by_income.png"), plot = plot_Income)

data.test.drfboro <- data.test %>% filter(drf_boro != "")
tau.hat.drfboro <- tau.hat[data.test$drf_boro != ""]

sort(unique(data.test.drfboro$drf_boro))
plot_drfboro <- HTE_plot(tau.hat = tau.hat.drfboro,data.test = data.test.drfboro,factor = "drf_boro", factor_label = c("Brooklyn", "Manhattan", "Queens", "Staten Island", "The Bronx"))
ggsave(file=paste0(Output, "HTE_by_dropoff.png"), plot = plot_drfboro)

