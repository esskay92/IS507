# Importing the necessary libraries
library(foreign)
library(CCA)
library(yacca)
library(MASS)


# Setting working directories
setwd("C:/Users/Siddhesh Unhavane/Desktop/Data Stat Info/Assignment")

responses = read.csv('responses.csv')
View(head(responses))

sum(is.na(responses))
responses <- na.omit(responses)

# Sub-setting the data frame for two seperate group of variables
music <- responses[,1:19]
spending <- responses[,134:140]

library(yacca)

# Performing the Canonical Correaltion Analysis or CCA
c2 = cca(music,spending)
c2

# Summarizing the CCA 
summary(c2)

# Performing CCA on the final project data set
football = read.csv("data.csv")
View(head(football))

