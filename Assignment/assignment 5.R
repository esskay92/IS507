#Libraries
library(DescTools)
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(REdaS) #Bartlett's Test of Sphericity
library(psych) #PCA/FA functions
library(factoextra) #PCA Visualizations
library("FactoMineR") #PCA functions
library(ade4) #PCA Visualizations

df <- read.csv('BIG5.csv')

# Question A
#Create PCA
p = prcomp(df, center=T, scale=T)
p

#Check Scree Plot
plot(p)
abline(1, 0)

# Question B
biplot(p) 

#Question C
p2 = psych::principal(df, rotate="varimax", nfactors=3, scores=TRUE)
p2
p3<-print(p2$loadings, cutoff=.47, sort=T)


# Question F
scores <- p2$scores
summary(scores)


#Question G
fit = factanal(df, 3)
print(fit$loadings, cutoff=.47, sort=T)
