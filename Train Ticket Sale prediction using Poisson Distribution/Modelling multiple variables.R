# Modelling Poisson Distrinution using Multiple variables

#Read training data
X <- read.csv("Train_Tickets.csv", header = T)

#Call Formatfile fun to calculate derived variables
Y <- Formatfile(X)

set.seed(200)
idx <- createDataPartition(Y$Count, p=0.25,list=FALSE)

YY <- data.frame(model.matrix(~.-1,Y[,c(2,3,6)]))
#Poisson based likelihood
hist(Y$Count, breaks = 50,probability = T ,main = "Histogram of Count Variable")
lines(density(Y$Count), col="red", lwd=2) 

X$dt <- as.POSIXct(X$Datetime, format="%d-%m-%Y %H:%M")

ggplot(data=X,aes(x=dt, y=Count)) + 
  geom_path(colour="blue") + 
  ylab("Count of Tickets Sold") + 
  xlab("Date") + 
  labs(title="Ticket Sale over time") 


ll<-function(theta0,theta1,theta2,theta3,theta4,theta5,theta6,theta7) {
  
  x1 <- YY$weekdayMonday[-idx]
  x2 <- YY$weekdayTuesday[-idx]
  x3 <- YY$weekdayWednesday[-idx]
  x4 <- YY$weekdayThursday[-idx]
  x5 <- YY$weekdayFriday[-idx]
  x6 <- YY$weekdaySaturday[-idx]
  x7 <- YY$age[-idx]
  
  y <- YY$Count[-idx]
  
  mu = exp(theta0 + x1*theta1 + x2*theta2 + x3*theta3 + x4*theta4+ x5*theta5+
             x6*theta6+x7*theta7)
  
  -sum(y*(log(mu)) - mu )
}

est <- stats4::mle(minuslog=ll, start=list(theta0=2,theta1=0,theta2=0,theta3=0,
                                           theta4=0,theta5=0,theta6=0,theta7=0))
#                   method = "L-BFGS-B")
summary(est)

pred.ts <- (exp(coef(est)['theta0'] + 
                  YY$weekdayMonday[idx]*coef(est)['theta1']+
                  YY$weekdayTuesday[idx]*coef(est)['theta2']+
                  YY$weekdayWednesday[idx]*coef(est)['theta3']+
                  YY$weekdayThursday[idx]*coef(est)['theta4']+
                  YY$weekdayFriday[idx]*coef(est)['theta5']+
                  YY$weekdaySaturday[idx]*coef(est)['theta6']+
                  YY$age[idx]*coef(est)['theta7'] ))

rmse(pred.ts, Y$Count[idx])
#78.54

lm.fit <-  lm(log(Count)~age, data=Y[-idx,])
pred.lm <- predict(lm.fit, Y[idx,])
rmse(exp(pred.lm), Y$Count[idx]) 
#87.734

