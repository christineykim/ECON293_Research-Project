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

list.of.packages <- c("tidyverse","dplyr","fastDummies","gtsummary","labelled","lmtest","sandwich","grf","glmnet","sandwich","splines","ggplot2","ggpubr","data.table","magrittr","qwraps2","rpart","MASS","pracma","haven","estimatr")
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

# Step 2: Applied LM-forest to the 1416 data -----
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

# Step 3: Generate the resulted statistics -----

## Estimate the ATE for the whole population 
lm.ATE.mean <- mean(tau.hat)
lm.ATE.SD <- sd(tau.hat)
ATE_stats <- c(lm.ATE.mean,lm.ATE.SD)
write.csv(ATE_stats,paste0(Output, "LM_forest_sumstats.csv"),row.names=F)

# Step 4: Generate the diagnostic plots -----

## 4.1. QINI plot -----

## Create a causal forest for plotting purposes  
cf.priority = causal_forest(X_train, Y_train, W_train)
priority.cate <- predict(cf.priority, X_test)$predictions

## Estimate the causal forest on the test data
cf.eval <- causal_forest(X_test, Y_test, W_test)

## Plot QINI curve - using rank_average_treatment_effect
rate <- rank_average_treatment_effect(cf.eval, tau.hat, target = "QINI")
png(file=paste0(Output, "LM_forest_QINI.png"),width=595, height=545)
plot(rate)
print(rate)
dev.off()

## 4.2. CATE distribution ----
png(file=paste0(Output, "LM_forest_CATE.png"),width=595, height=545)
hist(tau.hat, main = "", xlab = "CATE")
dev.off()

##4.3. Create quantile plots ----

## Set-up
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

## GGplot
cali_plot <- ggplot(ATE.df.fnl, aes(tau.mean, ATE)) +
  geom_point() + 
  geom_errorbar(aes(ymin = CI_max, ymax = CI_min)) + 
  theme_light() + labs(x = "Tau hat", y = "ATE", title = "") 

ggsave(file=paste0(Output,"Calibration_by_quartile_LM_forest.png"),plot = cali_plot)

cali_plot

# NOTE: The workspace with all data and estimations up to this point are stored in Output\lm_forest\lm_f_workspace_bruno
