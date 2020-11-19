#############################
## ECON 494 PROJECT 2 F20 ###
############################
df<-read.csv('/Users/victoriaroberts/Local\ Documents/ECON\ 494\ Intro\ to\ bus\ analytics/ECON_494_FINAL.csv')
clean_data<- read.csv('/Users/victoriaroberts/Local\ Documents/ECON\ 494\ Intro\ to\ bus\ analytics/ECON_494_FINAL.csv')
View(clean_data) #open data-summarized-in another tab

library(ggplot2) #for ggplot system and preloaded datasets
library(plyr) #for ddply()
library(tseries) #for the J-B test

##Transforming the Data##
clean_data$Public.spending2<-clean_data$Public.spending2^2 #Quadratic Transformation (2nd order)
clean_data$Public.spending3<-df$Public.spending^3 #CUBIC TRANSFORMATION (3rd ORDER)
clean_data$ln_Public.spending<-log(df$Public.spending)  #Logarithmic transformation


##############################
#Part 1:Partitioning the Data#
##############################
#Fraction of sample to be used for training
p<-.7   #Uses 70% of data to train/build model

#Number of observations (rows) in the dataframe
obs_count<-dim(clean_data)[1]

#Number of observations to be selected for the training Partition
training_size <- floor(p * obs_count)
training_size
#Set the seed to make the partition reproducible
set.seed(1234)
#Create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- clean_data[train_ind, ] #Pulls random rows for training
Testing <- clean_data[-train_ind, ] #Pulls random rows for testing

dim(Training)   #Checking the dimensions of the partitioned data
dim(Testing)
##DO WITH OWN DATA##FOR BOTTOM PLOTTING
#PLOTTING THE TRAINING AND TESTING PARTITIONS  
plot(Unemployment.rate ~ Public.spending, clean_data, xlim=c(1.5,7), ylim=c(10,45)) #Plot the entire dataset
plot(Unemployment.rate ~ Public.spending, Training, xlim=c(1.5,7), ylim=c(10,45), col ='blue') #PLOTS THE IN-SAMPLE TRAINING PARTITION
plot(Unemployment.rate ~ Public.spending, Testing, xlim=c(1.5,7), ylim=c(10,45),  col ='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Training$Public.spending, Training$Unemployment.rate, col='blue') #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Testing$Public.spending, Testing$Unemployment.rate, col='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION

######################
#Part 2: Build Models#
######################
#Building Linear Regression from the training data#

M1 <- lm(Unemployment.rate ~ Public.spending, Training)    #Building model from the training data
summary(M1)    #Generates summary diagnostic output

#Generating Predictions on the training data
PRED_1_IN <- predict(M1, Training)  #Generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(M1$fitted.values)  #Fitted values will be the same as the predicted

#Generating Predictions on the test data for benchmarking 
PRED_1_OUT <- predict(M1, Testing)  #Prediction (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$Unemployment.rate)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$Unemployment.rate)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M1, list(Public.spending=x_grid))
plot(Training$Unemployment.rate ~ Training$Public.spending, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Unemployment.rate ~ Testing$Public.spending, col='red', pch=3)

#Question:  Are the residuals normal?
hist(M1$residuals) #PLOT THEM!
jarque.bera.test(M1$residuals) #TEST FOR NORMLAITY!

#Building the Quadratic Model from the training data
M2 <- lm(Unemployment.rate ~ Public.spending + Public.spending2, Training)
summary(M2) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_2_IN <- predict(M2, Training) #(in-sample) prediction on training data
View(PRED_2_IN)
View(M2$fitted.values) 

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(M2, Testing) #(out-of-sample) predictions on testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$Unemployment.rate)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$Unemployment.rate)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M2, list(Public.spending=x_grid, Public.spending2=x_grid^2))
plot(Training$Unemployment.rate ~ Training$Public.spending, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Unemployment.rate ~ Testing$Public.spending, col='red', pch=3)

#Question:  Are the residuals normal?
hist(M2$residuals) #PLOT THEM!
jarque.bera.test(M2$residuals) #TEST FOR NORMLAITY!

#Building Logarithmic Regression with the training data
M3 <- lm(Unemployment.rate ~ ln_Public.spending, Training)
summary(M3) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(M3$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$Unemployment.rate)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$Unemployment.rate)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(M3, list(ln_Public.spending=log(x_grid)))
plot(Training$Unemployment.rate ~ Training$Public.spending, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$Unemployment.rate ~ Testing$Public.spending, col='red', pch=3)

#Question:  Are the residuals normal?
hist(M3$residuals) #PLOT THEM!
jarque.bera.test(M3$residuals) #TEST FOR NORMLAITY!

#Build multiple linear regression with training data#
M4 <- lm(Unemployment.rate ~ Public.spending + GDP + Inflation.rate + Population, clean_data)
summary(M4)

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(M1$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(M4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$Unemployment.rate)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$Unemployment.rate)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

#Question:  Are the residuals normal?
hist(M4$residuals) #PLOT THEM!
jarque.bera.test(M4$residuals) #TEST FOR NORMLAITY!

##########################
#Part 3: Model Comparison#
##########################

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN #MODEL WITH ONLY LINEAR TERM
RMSE_2_IN #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_IN #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_IN #LOGARITHMIC MODEL

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT #MODEL WITH ONLY LINEAR TERM
RMSE_2_OUT #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_OUT #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_OUT #LOGARITHMIC MODEL

