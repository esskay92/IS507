# Importing the libraries
library(plyr)
library(corrplot)
library(DescTools)


# Setting working directory
setwd('C:/Users/Siddhesh Unhavane/Desktop/Data Stat Info')




# Reading the dataset
pl <- read.csv('data.csv')
View(pl)

data <- pl[c('FTHG','FTAG','FTR','HTHG','HTAG','HS','AS','HST','AST','HF','AF','HC','AC','HY','AY','HR','AR')]
View(data)



# Correlation between various match statistics

# All statistics
corrplot(cor(data, method = 'spearman'))
corrplot(cor(data, method = 'spearman'),method = 'number')

# Home statistics
homeStats <- data[c('FTHG','HTHG','HS','HST','HF','HC','HY','HR','FTR')]
corrplot(cor(homeStats, method = 'spearman'),method = 'number')

# Away statistics
awayStats <- data[c('FTAG','HTAG','AS','AST','AF','AC','AY','AR','FTR')]
corrplot(cor(awayStats, method = 'spearman'),method = 'number')


homeStats <- homeStats.drop

# Linear regression model (ALL)
AllStatsModel <- lm(FTR ~ ., data = data)
AllStatsModel
VIF(AllStatsModel)
par(mfrow = c(2,2))
plot(AllStatsModel)
summary(AllStatsModel)

# Linear regression model (Home)
HomeStatsModel <- lm(FTR ~ ., data = homeStats)
HomeStatsModel
VIF(HomeStatsModel)
par(mfrow = c(2,2))
plot(HomeStatsModel)
summary(HomeStatsModel)

# Linear regression model (Away)
AwayStatsModel <- lm(FTR ~ ., data = awayStats)
AwayStatsModel
VIF(AwayStatsModel)
par(mfrow = c(2,2))
plot(AwayStatsModel)
summary(AwayStatsModel)

