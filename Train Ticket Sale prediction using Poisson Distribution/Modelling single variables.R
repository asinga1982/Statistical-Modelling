# Modelling Poisson Distrinution using Singla variables
library(dplyr)
library(ggplot2)
library(caret)
library(ModelMetrics)
library(stats4)
#Read training data

X <- read.csv("Train_Tickets.csv", header = T)

#Call Formatfile fun to calculate derived variables
Y <- Formatfile(X)

#Plot density of the sale
hist(Y$Count, breaks = 50,probability = T ,main = "Histogram of Count Variable")
lines(density(Y$Count), col="red", lwd=2) 

X$dt <- as.POSIXct(X$Datetime, format="%d-%m-%Y %H:%M")

#Plot of Sale over time
ggplot(data=X,aes(x=dt, y=Count)) + 
  geom_path(colour="blue") + 
  ylab("Count of Tickets Sold") + 
  xlab("Date") + 
  labs(title="Ticket Sale over time") 

#Partition data into test and train set
set.seed(200)
idx <- createDataPartition(Y$Count, p=0.25,list=FALSE)

#Negative log likelihood Function using Poisson Distribution
nll <- function(theta0,theta1) {
  
  x <- Y$age[-idx]
  y <- Y$Count[-idx]
  
  mu = exp(theta0 + x*theta1)
  
  -sum(y*(log(mu)) - mu)
}

#Parameter Estimation
est <- stats4::mle(minuslog=nll, start=list(theta0=2,theta1=0))
summary(est)

#Prediction on test set and RMSE
pred.ts <- (exp(coef(est)['theta0'] + Y$age[idx]*coef(est)['theta1'] ))
rmse(pred.ts, Y$Count[idx])
#86.95227

#Comparison with standard linear model
lm.fit <-  lm(log(Count)~age, data=Y[-idx,])
pred.lm <- predict(lm.fit, Y[idx,])
rmse(exp(pred.lm), Y$Count[idx]) 
#93.77393