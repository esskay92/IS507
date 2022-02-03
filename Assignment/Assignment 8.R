# LIBRARY IMPORTS
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(rsample)
library(caret)
library(vip)
library(ROCR)
library(precrec)
library(ROCit)


# Setting up working directory
setwd('C:/Users/Siddhesh Unhavane/Desktop/Data Stat Info/Assignment')


# Reading the csv file
emp <- read.csv('employee.csv')


# Checking if the data is correctly read or not
View(emp)
# The data is appropriately loaded into the environment


# Checking the dimension of the dataset
dim(emp)
# We can see the dataset consists of 4653 rows and 9 columns


# Checking variable names
names(emp)


# Checking overall data structure of the dataset
str(emp)


# Checking if there is any missing data within the dataset
sum(is.na(emp))
# There are no null values. Hence we can proceed further with the dataset


# Converting 'Education' to numerical variable
emp$Education <- revalue(emp$Education, c("Bachelors"='1',"Masters"='2',"PHD"='3'))
emp$Education <- as.numeric(emp$Education)
View(head(emp))


# Converting 'City' to numerical variable
unique(emp$City)
emp$City <- revalue(emp$City, c("Bangalore"='1',"Pune"='2',"New Delhi"='3'))
emp$City <- as.numeric(emp$City)
View(head(emp))


# Converting 'Gender' to numerical variable
unique(emp$Gender)
emp$Gender <- revalue(emp$Gender, c("Male"='1',"Female"='2'))
emp$Gender <- as.numeric(emp$Gender)
View(head(emp))


# Converting 'EverBenched' to numerical variable
unique(emp$EverBenched)
emp$EverBenched <- revalue(emp$EverBenched, c("Yes"='1',"No"='0'))
emp$EverBenched <- as.numeric(emp$EverBenched)
View(head(emp))


# Creating a 70-30 split for training and testing data respectively
set.seed(123)
split <- initial_split(emp, prop = .7, strata = "LeaveOrNot")
train <- training(split)
test  <- testing(split)

#----------------------------------------------------------------------------------------------------------------------

# QUESTION 1(a)
emp$LeaveOrNot <- as.factor(emp$LeaveOrNot)

# Creating a logistic regression model with LeaveOrNot as the target variable
log_reg <- glm(
  LeaveOrNot ~ Education + JoiningYear + City + PaymentTier + Age + Gender + EverBenched + ExperienceInCurrentDomain,
  family = 'binomial',
  data = emp
)

# Getting the summary of the logistic regression model
summary(log_reg)
tidy(log_reg)

# Coefficients in exponential form
log_reg %>% 
  gtsummary::tbl_regression(exp = TRUE) 


# We can clearly see the odds ratio of all the variables
# The basic concept of odds ratio is that when the variable is greater than 1, it promotes the happening of outcome.
# When the odds ratio is less than 1, it prevents the happening of outcome.
# In this case Education, JoiningYear, Gener, EverBenched are promoting leaving of a candidate.
# On the other hand City, PaymentTier, Age, ExperienceInCurrentDomain are preventing the leaving of a candidate.

#----------------------------------------------------------------------------------------------------------------------

# QUESTION 1(b)

train$LeaveOrNot<- as.factor(train$LeaveOrNot)

# Training the model
log_reg = train(
  form = LeaveOrNot ~ Education + JoiningYear + City + PaymentTier + Age + Gender + EverBenched + ExperienceInCurrentDomain,
  data = train,
  method = 'glm',
  family = 'binomial'
)

# Predicting the dependent variable
pred <- predict(log_reg, test)
pred

# Confusion Matrix
confusionMatrix(pred, as.factor(test$LeaveOrNot))

# From the confusion matrix, we can conclude that accuracy of the model is 68.41%
# This accuracy is fairly low and the model has further scope of improvement.

#------------------------------------------------------------------------------------------------------------------------

# QUESTION 1(c)

log_reg_train <- glm(LeaveOrNot ~ Education + JoiningYear + City + PaymentTier + Age + Gender + EverBenched + ExperienceInCurrentDomain,
                     data=train, family=binomial)

log_reg_test_prob <- log_reg_train %>% predict(test, type = "response")
log_reg_test_prob

preds <- prediction(as.numeric(log_reg_test_prob), test$LeaveOrNot)

perf <- performance(preds,"tpr","fpr")
plot(perf,colorize=TRUE)

precrec_obj <- evalmod(scores = log_reg_test_prob, labels = test$LeaveOrNot)
autoplot(precrec_obj)

# Getting the AUC scores
sm_aucs <- auc(precrec_obj)
sm_aucs

precrec_obj2 <- evalmod(scores = log_reg_test_prob, labels = test$LeaveOrNot, mode="basic")
autoplot(precrec_obj2) 

ROCit_obj <- rocit(score=log_reg_test_prob,class=test$LeaveOrNot)
plot(ROCit_obj)

summary(ROCit_obj)

measure <- measureit(score = log_reg_test_prob, class = test$LeaveOrNot,
                     measure = c("ACC", "MIS", "SENS", "SPEC", "PREC", "REC","PPV","NPV", "FSCR"))
measure

#---------------------------------------------------------------------------------------------------------------------

# QUESTION 1(d)

# We create a binary classifier model that is a logistic regression model. This model helps in correctly 
# classifying the binary outcome variable based on one or more independent variables. Part A has it's final outcome in 
# the form of Odds Ratio. Odds ratio tell us how variables effect the outcome. If the odds ratio of a variable is 
# greater than 1, the variable contributes to the likelihood of making the outcome happen. On the other hand if
# the odds ratio of a variable is less than 1, then contributes to the likelihood of preventing the outcome from
# happening. In Part B, we create a confusion matrix. A confusion matrix is a matrix that describes the classification 
# of the outcomes predicted by our model. The classifications are mainly classified as True Positives(TP), True Negatives
# (TN), False Positives(FP), and False Negatives(FN). The ratio of these in various combination gives us the performance
# metrics of the model. The metrics talks about precision, recall, f1-score, support, etc.


#---------------------------------------------------------------------------------------------------------------------

# QUESTION 1(e)

# In Assignment we performed a LDA which stands for Linear Discriminant Analysis. Since LDA is linear, it is used 
# in prediction of continuous or linear variables or classification of variables linearly. LDA looked for a particular
# linear combination of feature variables that best explained the target variable. After performing the LDA, we performed
# a cross validation.


# Where as in this assignment we performed and learned about logistic regression. Logistic regression is generally used
# when the outcome variable or a target variable is binary and categorical. Logistic regression predicted the 
# binary outcome on the basis of numerous feature variables with a regression at its core. Logistic regression being a 
# machine learning algorithm required a large amount of data to train the regression model. No cross validation was
# performed in logistic regression.


