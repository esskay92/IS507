library(Hmisc)
library(plyr)

# Loading the dataset
data <- read.csv('Student_Grade_Prediction.csv',header = TRUE)
head(data)
View(data)

# Summary of the dataset
summary(data)
describe(data)

# Data cleaning
sum(is.na(data))

# Since the number of NA, NUll, or missing values in the dataset is 0, there is no need to further clean the dataset.

######################################################################################################################################################

# QUESTION 1

# Converting the internet column into numerical variables
data$internet = revalue(data$internet, c('no'= 0,'yes'= 1))
data$internet = as.numeric(data$internet)

# Checking the normality to determine whether to use parametric test or non-parametric test.

shapiro.test(data$internet)

# The p-value of internet is 2.2e-16 which is less than 0.05.
# Hence the internet is not normal, we have to perform a non parametric test.

# Since the independent variable is continuous and the dependent variable is binary(categorical) 
# and the data is not normal, we have to perform Mann-Whitney U test.
wilcox.test(data$G3 ~ data$internet)

# After this test, we found that the p-value is 0.0324 which is less than 0.05. 
# Hence we reject the null hypothesis and accept the alternate hypothesis. 
# There is a difference for G3 final grades between students with internet from students without internet.

#########################################################################################################################################################

# QUESTION 2

# First we will need to convert the categorical variables to numeric variables
data$higher = revalue(data$higher, c('no'= 0,'yes'= 1))
data$higher = as.numeric(data$higher)
data$activities = revalue(data$activities, c('no'= 0,'yes'= 1))
data$activities = as.numeric(data$activities)

# Since both the variables are binary, we have to use chi square and fisher test and determine the outcome.
library(gmodels)
CrossTable(data$higher,data$activities,digits = 2, expected=TRUE, prop.r=TRUE, 
           prop.c = TRUE, prop.chisq = FALSE, chisq = TRUE, fisher = TRUE, format="SPSS")

# From the cross table, it is clear that the Minimum Expected Frequency is 9.28 which is greater than 5. 
# Hence we consider the p-value from chi squared test.
# In chi squared test, the p-value is 0.05516458 which is greater than 0.05.
# Hence we accept the null hypothesis and reject the alternative hypothesis. 
# Thus, there is no relationship between activities and higher education.

###########################################################################################################################################################################

# QUESTION 3

shapiro.test(data$G2)
shapiro.test(data$G3)

# It is clear that the data is not normal but it is paired. Hence we will use Paired Wilcoxon Sign Rank Test 
wilcox.test(data$G2,data$G3, paired = TRUE)
    
# since the p-value is greater than 0.05, we accept the null hypothesis which says that there is no significant difference between g2 and g3

############################################################################################################################################################

# QUESTION 4
data$G2_cat<-NULL
data$G2_cat[(data$G2 > 15)] <- 1
data$G2_cat[(data$G2 <= 15)] <- 0

data$G3_cat<-NULL
data$G3_cat[(data$G3 > 15)] <- 1
data$G3_cat[(data$G3 <= 15)] <- 0

shapiro.test(data$G2_cat)
shapiro.test(data$G3_cat)

# Since the data is not normal and both are categorical data and binary variables, we will use McNemar test.
library(gmodels)

CrossTable(data$G2_cat,data$G3_cat,digits = 2, prop.r=TRUE, 
           prop.c = TRUE, format="SPSS")


mcnemar.test(data$G2_cat,data$G3_cat)


# Since the p-value is greater than 0.05, we have to accept the null hypothesis and reject the alternate hypothesis.
# Thus there is no difference between the two categorical variables.

###############################################################################################################################################################################################################

# QUESTION 5
table(data$schoolsup)

n = 344 + 51
prop.test(x = 51,n = 395, correct = FALSE)
prop.test(x = 51,n = 395, correct = FALSE, conf.level = 0.50)

# The probability of students receiving educational support is same for both 95% and 50% 
# But the confidence interval changes when we change the confidence level

#########################################################################################################################################################################################################################

# QUESTION 6

prop.test(table(data$sex, data$activities), correct=FALSE,conf.level = 0.95)
CrossTable(data$sex, data$activities)

#######################################################################################################################################################


