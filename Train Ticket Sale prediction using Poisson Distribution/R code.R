setwd("E:/R Directory/Train ticket prediction - AV")
library(dplyr)

X <- read.csv("Train_Tickets.csv")
A <- read.csv("Test_Tickets.csv")


Y <- Formatfile(X)
fit <- lm(sqrt(sqrt(Count))~age + thr + weekday + Mon + Day + thr*weekday, Y)

## Test file read in A
Y <- formattest(A)
P <- predict.lm(fit, Y)
Sub <- data.frame(A$Datetime, round(P^4))
write.csv(Sub, "Sample Submission1.csv")
