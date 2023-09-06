rm(list=ls())

library(gbm)
library(ROCR)
library(caret)
library(mboost)
library(dplyr)
library(ggplot2)
library(margins)
library(ROSE)

######################### CORRPLOT ############################

data_df <-read.csv("data_curate_withy.csv")

data <- select(data_df, largeretail, smallretail, fastfood, fullservice,
               hh_medianincome000, pov_rate, hh_snap, tract_gini, 
               unemp, below_hs, no_degree, grad, hh_medianval000, pub_transport, hh_novehicle, area, pop_dens, black_pop,
               hispanic_pop, asian_pop, native_pop, pacific_pop, wfh, rural_perc, walkability, transit_sqml,
               network_density_auto, food_tax, bc_density, wc_density, crime)

data <- data[complete.cases(data), ]

data %>% rstatix::get_summary_stats(type = "mean_sd") -> tab

ggcorrplot::ggcorrplot(round(cor(data[,-which(colnames(data)==c("largeretail", "smallretail", "fastfood", 
                                                                "fullservice"))]),2),"square","upper",lab=T,lab_size=3,
                       title=paste0("Correlation of Predictors")) -> corplot

corplot + 
  scale_x_discrete(labels = c("HH median income", "Poverty rate", "HH with SNAP", "Inequality", 
                              "Unemployment", "Below high school", "No degree", "College graduate", "Median property value",
                              "Public transport", "HH no vehicle", "Area", "Population density", "Black", "Hispanic", "Asian", 
                              "Native", "Pacific Islander", "Work from home", "Rural", 
                              "Walkability", "Transit density", "Auto network density", "Food sales tax", "Blue collar job density",
                              "White collar job density")) + 
  scale_y_discrete(labels = c("Poverty rate", "HH with SNAP", "Inequality", 
                              "Unemployment", "Below high school", "No degree", "College graduate", "Median property value",
                              "Public transport", "HH no vehicle", "Area", "Population density", "Black", "Hispanic", "Asian", 
                              "Native", "Pacific Islander", "Work from home", "Rural", 
                              "Walkability", "Transit density", "Auto network density", "Food sales tax", "Blue collar job density",
                              "White collar job density", "Crime"))

######################### T-TEST ############################

data_df <-read.csv("data_curate_withy.csv")

data <- select(data_df, largeretail, smallretail, fastfood, fullservice,
               hh_medianincome000, pov_rate, hh_snap, tract_gini, 
               unemp, below_hs, no_degree, grad, hh_medianval000, pub_transport, hh_novehicle, area, pop_dens, area, black_pop,
               hispanic_pop, asian_pop, native_pop, pacific_pop, wfh, rural_perc, walkability, transit_sqml,
               network_density_auto, food_tax, bc_density, wc_density, crime)

data <- data[complete.cases(data), ]

meanM=NULL
meanY=NULL
t_stat=NULL

depvar <- "fullservice" #note to self: remember to change dependent variable!

for (i in 5:(ncol(data))){
  meanM=as.data.frame(rbind(meanM, t.test(data[,i] ~ data[,depvar])$estimate[1]))
  meanY=as.data.frame(rbind(meanY, t.test(data[,i] ~ data[,depvar])$estimate[2]))
  diff=meanY-meanM
  t_stat=as.data.frame(rbind(t_stat, t.test(data[,i] ~ data[,depvar])$statistic))
}

t_test <- cbind(diff, t_stat)

######################### STORE PRESENCE ############################

######################### BOOSTED LOGIT W HYPERPARAMETER TUNING ############################

data_df <-read.csv("data_curate_withy.csv")

data <- select(data_df, largeretail, smallretail, fastfood, fullservice, 
               hh_medianincome000, pov_rate, hh_snap, tract_gini, 
               unemp, below_hs, no_degree, grad, hh_medianval000, pub_transport, hh_novehicle, area, pop_dens, area, black_pop, white_pop,
               hispanic_pop, asian_pop, native_pop, pacific_pop, wfh, rural_perc, walkability, transit_sqml,
               network_density_auto, food_tax, bc_density, wc_density, crime)

data <- data[complete.cases(data), ]

df_pred <- data.frame(matrix(ncol = 0, nrow = nrow(data))) #df to store predicted probabilities for ggarrange plot

#change dependent variables and run boosted logit  

depvar <- "fullservice" #note to self: remember to change the dependent variable!

data[,depvar] <- as.factor(data[,depvar])

set.seed(12345)

train_control <- trainControl(method = "cv", number = 10, savePredictions = "final")
intrain <- createDataPartition(data[,depvar],p=0.7,list=FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

hyper_grid <- expand.grid(
  shrinkage = c(0.01, 0.05, 0.10),
  interaction.depth = c(1,5,10),
  n.trees = c(100,300,500),
  n.minobsinnode=c(100)
)

gbm_model = train(fullservice~.-white_pop, data=training, #note to self: remember to change the dependent variable
                  method = "gbm",
                  trControl = train_control,
                  tuneGrid = hyper_grid,
                  verbose=FALSE)

summary(gbm_model)

yhat_gbm=predict(gbm_model, newdata = data)
confusionMatrix(as.factor(testing[,depvar]),yhat_gbm[-intrain])
confusionMatrix(as.factor(training[,depvar]),yhat_gbm[intrain])

ggplot(gbm_model)

df_pred[,depvar] <- predict(gbm_model, newdata = data, type="prob")[,2]*100 #note to self: go back and run with a different dependent variable

#add white_pop and hh_medianincome000 for ggarrange plot

df_pred$white_pop <- data$white_pop
df_pred$hh_medianincome000 <- data$hh_medianincome000

#plot against race

largeretail_wpred <- ggplot(df_pred, aes(x = white_pop, y = largeretail)) + geom_point(alpha = 0.05)+
  geom_smooth(method = "lm", se=TRUE,linetype="dashed", color="steelblue") + labs(x="White population (%)", y= "Large retailer probability (%)",
                                                                                  title="Large retailer presence vs White population") 

smallretail_wpred <- ggplot(df_pred, aes(x = white_pop, y = smallretail)) + geom_point(alpha = 0.05)+
  geom_smooth(method = "lm", se=TRUE,linetype="dashed", color="steelblue") + labs(x="White population (%)", y= "Small retailer probability (%)",
                                                                                  title="Small retailer presence vs White population") 

fastfood_wpred <- ggplot(df_pred, aes(x = white_pop, y = fastfood)) + geom_point(alpha = 0.05)+
  geom_smooth(method = "lm", se=TRUE,linetype="dashed", color="steelblue") + labs(x="White population (%)", y= "Fast food probability (%)",
                                                                                  title="Fast food presence vs White population")

fullservice_wpred <- ggplot(df_pred, aes(x = white_pop, y = fullservice)) + geom_point(alpha = 0.05)+
  geom_smooth(method = "lm", se=TRUE,linetype="dashed", color="steelblue") + labs(x="White population (%)", y= "Full service probability (%)",
                                                                                  title="Full service presence vs White population")

ggpubr::ggarrange(largeretail_wpred, smallretail_wpred, fastfood_wpred, fullservice_wpred, 
          ncol = 2, nrow = 2)

#plot against income

largeretail_ipred <- ggplot(df_pred, aes(x = hh_medianincome000, y = largeretail)) + geom_point(alpha = 0.05)+
  geom_smooth(method = "lm", se=TRUE,linetype="dashed", color="steelblue") + labs(x="Median income ($ '000s)", y= "Large retailer probability (%)",
                                                                                  title="Large retailer presence vs Income") 

smallretail_ipred <- ggplot(df_pred, aes(x = hh_medianincome000, y = smallretail)) + geom_point(alpha = 0.05)+
  geom_smooth(method = "lm", se=TRUE,linetype="dashed", color="steelblue") + labs(x="Median income ($ '000s)", y= "Small retailer probability (%)",
                                                                                  title="Small retailer presence vs Income") 

fastfood_ipred <- ggplot(df_pred, aes(x = hh_medianincome000, y = fastfood)) + geom_point(alpha = 0.05)+
  geom_smooth(method = "lm", se=TRUE,linetype="dashed", color="steelblue") + labs(x="Median income ($ '000s)", y= "Fast food probability (%)",
                                                                                  title="Fast food presence vs Income")

fullservice_ipred <- ggplot(df_pred, aes(x = hh_medianincome000, y = fullservice)) + geom_point(alpha = 0.05)+
  geom_smooth(method = "lm", se=TRUE,linetype="dashed", color="steelblue") + labs(x="Median income ($ '000s)", y= "Full service probability (%)",
                                                                                  title="Full service presence vs Income")

ggpubr::ggarrange(largeretail_ipred, smallretail_ipred, fastfood_ipred, fullservice_ipred, 
                  ncol = 2, nrow = 2)


######################### STANDARD LOGIT ############################

data_df <-read.csv("data_curate_withy.csv")

data <- select(data_df, largeretail, smallretail, fastfood, fullservice, 
               hh_medianincome000, pov_rate, hh_snap, tract_gini, 
               unemp, below_hs, no_degree, grad, hh_medianval000, pub_transport, hh_novehicle, area, pop_dens, area, black_pop, white_pop,
               hispanic_pop, asian_pop, native_pop, pacific_pop, wfh, rural_perc, walkability, transit_sqml,
               network_density_auto, food_tax, bc_density, wc_density, crime)

data <- data[complete.cases(data), ]

depvar <- "fullservice" #note to self: remember to change the dependent variable!

set.seed(12345)

train_control <- trainControl(method = "cv", number = 10, savePredictions = "final")
intrain <- createDataPartition(data[,depvar],p=0.7,list=FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

gbmmodel_3 <- glm(as.factor(fullservice) ~ .-white_pop, data = training, family = binomial(link="logit")) #note to self: remember to change the dependent variable!
results_pred_3 <- predict(gbmmodel_3,type="response", newdata=data)
pred_results_3 <- as.factor(ifelse(results_pred_3>0.50,1,0))
confusionMatrix(as.factor(testing[,depvar]),pred_results_3[-intrain])
margins(gbmmodel_3)

######################### BOOSTED LOGIT W BALANCING ############################

data_df <-read.csv("data_curate_withy.csv")

data <- select(data_df, largeretail, smallretail, fastfood, fullservice, 
               hh_medianincome000, pov_rate, hh_snap, tract_gini, 
               unemp, below_hs, no_degree, grad, hh_medianval000, pub_transport, hh_novehicle, area, pop_dens, area, black_pop,
               hispanic_pop, asian_pop, native_pop, pacific_pop, wfh, rural_perc, walkability, transit_sqml,
               network_density_auto, food_tax, bc_density, wc_density, crime)

data <- data[complete.cases(data), ]

depvar <- "fullservice"

set.seed(12345)

train_control <- trainControl(method = "cv", number = 10, savePredictions = "final")
intrain <- createDataPartition(data[,depvar],p=0.7,list=FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

table(training[,depvar])

data_balanced <- ovun.sample(fullservice ~ ., data = training, method = "both", p=0.5, N=nrow(training), seed = 12345)$data #remember to change dep var!
table(data_balanced[,depvar])

set.seed(12345)

hyper_grid <- expand.grid(
  shrinkage = c(0.01, 0.05, 0.10),
  interaction.depth = c(1,5,10),
  n.trees = c(100,300,500),
  n.minobsinnode=c(100)
)

gbmmodel_balanced = train(fullservice~.-white_pop, data=training, #note to self: remember to change the dependent variable
                  method = "gbm",
                  trControl = train_control,
                  tuneGrid = hyper_grid,
                  verbose=FALSE)

summary(gbmmodel_balanced)

results_pred <- predict(object=gbmmodel_balanced, n.trees=gbm.perf(gbmmodel_balanced, method="cv"), type="response", newdata=data)
pred_results <- as.factor(ifelse(results_pred>0.50,1,0))
confusionMatrix(as.factor(testing[,depvar]),pred_results[-intrain])

######################### STORE COUNT GROWTH ############################

######################### BOOSTED OLS W HYPERPARAMETER TUNING ############################

data_df <-read.csv("data_curate_withy.csv")

depvar <- "largeretail_count_growth"

data <- select(data_df, largeretail_count_growth, smallretail_count_growth, fastfood_count_growth, fullservice_count_growth, 
               hh_medianincome000, pov_rate, hh_snap, tract_gini, 
               unemp, below_hs, no_degree, grad, hh_medianval000, pub_transport, hh_novehicle, area, pop_dens, area, black_pop,
               hispanic_pop, asian_pop, native_pop, pacific_pop, wfh, rural_perc, walkability, transit_sqml,
               network_density_auto, food_tax, bc_density, wc_density, crime)

data <- data[complete.cases(data), ]

set.seed(12345)

train_control <- trainControl(method = "cv", number = 10, savePredictions = "final")
intrain <- createDataPartition(data$fullservice_count_growth,p=0.7,list=FALSE) #note to self: remember to change the dependent variable!
training <- data[intrain,]
testing <- data[-intrain,]

hyper_grid <- expand.grid(
  shrinkage = c(0.01, 0.05, 0.10),
  interaction.depth = c(1,5,10),
  n.trees = c(100,300,500),
  n.minobsinnode=c(100)
)

gbm_model = train(fullservice_count_growth ~., data=training, distribution="gaussian", #note to self: remember to change the dependent variable!
                  method = "gbm",
                  trControl = train_control,
                  tuneGrid = hyper_grid,
                  verbose=FALSE)
summary(gbm_model)

yhat_gbm <- predict(gbm_model, newdata=data)

ggplot(gbm_model)

RMSE(testing[,depvar], yhat_gbm[-intrain])/sd(testing[,depvar])
RMSE(training[,depvar], yhat_gbm[intrain])/sd(training[,depvar])

######################### STANDARD OLS ############################

data_df <-read.csv("data_curate_withy.csv")

depvar <- "fullservice_count_growth" #note to self: remember to change the dependent variable!

data <- select(data_df, largeretail_count_growth, smallretail_count_growth, fastfood_count_growth, fullservice_count_growth, 
               hh_medianincome000, pov_rate, hh_snap, tract_gini, 
               unemp, below_hs, no_degree, grad, hh_medianval000, pub_transport, hh_novehicle, area, pop_dens, area, black_pop,
               hispanic_pop, asian_pop, native_pop, pacific_pop, wfh, rural_perc, walkability, transit_sqml,
               network_density_auto, food_tax, bc_density, wc_density, crime)

data <- data[complete.cases(data), ]

set.seed(12345)

train_control <- trainControl(method = "cv", number = 10, savePredictions = "final")
intrain <- createDataPartition(data[,depvar],p=0.7,list=FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

summary(gbmmodel_3 <- lm(fullservice_count_growth ~ ., data = training)) #note to self: remember to change the dependent variable!
results_pred_3 <- predict(gbmmodel_3, newdata=data)
RMSE(testing[,depvar], results_pred_3[-intrain])/sd(testing[,depvar])
RMSE(training[,depvar], results_pred_3[intrain])/sd(training[,depvar])

























