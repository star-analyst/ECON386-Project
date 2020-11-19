################################################
##Final Project - Predictive Analytics Project##
################################################

#IMPORT THE RAW DATA SET
performance<-read.csv('https://raw.githubusercontent.com/star-analyst/ECON386-Project/main/StudentsPerformance_Raw.csv')

#LOAD LIBRARIES
library(ggplot2)
library(plyr)
library(tseries)

#CHECK STATUS
dim(performance)#Check the dimension of the data frame
summary(performance[,1:8])#Generates summary statistics for all variables
View(performance)#View data in a spreadsheet format
head(performance)#View first six rows of the data frame

#RENAME COLUMNS
names(performance)[names(performance) == "race.ethnicity"] <- "race" #rename race/ethnicity to race
names(performance)[names(performance) == "parental.level.of.education"] <- "parentEdu" #rename parental level of education to parent education
names(performance)[names(performance) == "test.preparation.course"] <- "testPrep" #rename test preparation course to test prep
names(performance)[names(performance) == "math.score"] <- "math" #rename math.score to math
names(performance)[names(performance) == "reading.score"] <- "reading" #rename reading.score to reading
names(performance)[names(performance) == "writing.score"] <- "writing" #rename writing.score to writing
head(performance)

#Look at histogram for each numerical variable#
hist(performance$math)#generates histogram for the math variable
hist(performance$reading)#generates histogram for the reading variable
hist(performance$writing)#generates histogram for the writing variable

#NOW DATA IS TIDY
summary(performance[,1:8])#Generate summary statistics in the tidy data frame
dim(performance)#Check the dimension of the tidy data frame 
write.table(performance)
#UPLOAD TO DESKTOP
write.csv(performance, "~/Desktop/StudentsPerformance_Tidy.csv")

#EXPLORATORY ANALYSIS
#SCATTER PLOTS USING THE POINT GEOM AND ADD A SMOOTHER TO A PLOT
ggplot(performance, aes(math, reading)) + geom_point() + geom_smooth()
ggplot(performance, aes(math, writing)) + geom_point()+ geom_smooth()
ggplot(performance, aes(reading, writing)) + geom_point() + geom_smooth()

#SCATTER PLOTS WITH COLORS
ggplot(performance, aes(math, reading, color = gender)) + geom_point()
ggplot(performance, aes(math, writing, color = gender)) + geom_point() 
ggplot(performance, aes(reading, writing, color = gender)) + geom_point() 

ggplot(performance, aes(math, reading, color = lunch)) + geom_point() 
ggplot(performance, aes(math, writing, color = lunch)) + geom_point() 
ggplot(performance, aes(reading, writing, color = lunch)) + geom_point() 

ggplot(performance, aes(math, reading, color = testPrep)) + geom_point() 
ggplot(performance, aes(math, writing, color = testPrep)) + geom_point() 
ggplot(performance, aes(reading, writing, color = testPrep)) + geom_point() 

#LINEAR REGRESSION MODEL
#MODEL1: math = B0 + B1*reading+u
MODLE1<-lm(math ~ reading, performance) #BUILD THE MODEL OBJECT USING lm()
summary(MODLE1) #REPORT SUMMARY OUTPUT OF THE MODEL OBJECT

#MODEL2 math = B0 + B1*writing+u
MODLE2 <- lm(math ~ writing, performance)
summary(MODLE2)

#MODEL3 MULTIPLE VARIABLE MODEL: math = B0 + B1*reading+ B2*writing + u
MODLE3 <- lm(math ~ reading + writing, performance)
summary(MODLE3)

#MODEL4: DUMMY VARIABLE MODEL: math = B0 + B1*gender + u
MODLE4 <- lm(math ~ gender, performance)
summary(MODLE4)

#PARTITION 
#fraction of sample to be used for training
p<-.7 #use 70% of the data to train/build the model

#number of observations (rows) in the data frame
obs_count<-dim(performance)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original data set
train_ind <- sample(obs_count, size = training_size)

Training <- performance[train_ind, ] #pulls random rows for training
Testing <- performance[-train_ind, ] #pulls random rows for testing

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)

#BUILDING THE MODEL FROM THE TRAINING DATA
#Model1
M1 <- lm(math ~ reading, Training)
summary(M1) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$math)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$math)^2)/length(PRED_1_OUT)) #computes out-of-sample 

#Model2
M2 <- lm(math ~ writing, Training)
summary(M2) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(M2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$math)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$math)^2)/length(PRED_2_OUT)) #computes out-of-sample 

#Model3
M3 <- lm(math ~ reading + writing, Training)
summary(M3) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$math)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$math)^2)/length(PRED_3_OUT)) #computes out-of-sample 

#Model4
M4 <- lm(math ~ gender, Training)
summary(M4) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(M4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$math)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$math)^2)/length(PRED_4_OUT)) #computes out-of-sample

#RESIDUAL NORMALITY 
jarque.bera.test(M1$residuals) 
jarque.bera.test(M2$residuals)
jarque.bera.test(M3$residuals)
jarque.bera.test(M4$residuals)

#IN-SAMPLE ERROR
RMSE_1_IN 
RMSE_2_IN
RMSE_3_IN
RMSE_4_IN

#OUT-OF-SAMPLE ERROR
RMSE_1_OUT 
RMSE_2_OUT 
RMSE_3_OUT 
RMSE_4_OUT 

#IMPORT THE TIDY DATA SET
performance<-read.csv('https://raw.githubusercontent.com/star-analyst/ECON386-Project/main/StudentsPerformance_Tidy.csv')
View(performance)

#CLASSIFICATION TASK
library(caret) #data partitioning library and other machine learning tools
library(rpart) #CART library
library(e1071) #svm library
library(randomForest) #random forest

#CREATING A NEW BINARY VARIABLE CALLED "sex" FOR gender CATEGORY
performance$sex <- NA #INITIALIZE NEW COLUMN
for (i in 1:length(performance$gender)) {
  if (performance$gender[i]=='female') {
    performance$sex[i] <- "0"
  } else {
    performance$sex[i] <- "1"
  }
}

performance$sex<-factor(performance$sex) #convert admit to factor (categorical) variable
View(performance)

#PARTITIONING DATA
set.seed(123) #locks seed for random partitioning
#creates a vector of rows to randomly sample from the raw data
inTrain <- createDataPartition(y=performance$sex, p=.70, list = FALSE) 
#stores these rows in the training set
Training<-performance[inTrain,]  
#stores all rows not in the training set in the test/validation set
Testing<-performance[-inTrain,] 

#LOGISTIC REGRESSION MODEL 1
M_LOG1<-glm(sex ~ math, data = Training, family = "binomial")
summary(M_LOG1)
exp(cbind(M_LOG1$coefficients, confint(M_LOG1)))
confusionMatrix(table(predict(M_LOG1, Testing, type="response") >= 0.5,
                      Testing$sex == 1), positive='TRUE')

#LOGISTIC REGRESSION MODEL 2
M_LOG2<-glm(sex ~ reading, data = Training, family = "binomial")
summary(M_LOG2)
exp(cbind(M_LOG2$coefficients, confint(M_LOG2)))
confusionMatrix(table(predict(M_LOG2, Testing, type="response") >= 0.5,
                      Testing$sex == 1), positive='TRUE')

#LOGISTIC REGRESSION MODEL 3
M_LOG3<-glm(sex ~ writing, data = Training, family = "binomial")
summary(M_LOG3)
exp(cbind(M_LOG3$coefficients, confint(M_LOG3)))
confusionMatrix(table(predict(M_LOG3, Testing, type="response") >= 0.5,
                      Testing$sex == 1), positive='TRUE')

#LOGISTIC REGRESSION MODEL 4
M_LOG4<-glm(sex ~ math+reading, data = Training, family = "binomial")
summary(M_LOG4)
exp(cbind(M_LOG3$coefficients, confint(M_LOG4)))
confusionMatrix(table(predict(M_LOG4, Testing, type="response") >= 0.5,
                      Testing$sex == 1), positive='TRUE')

