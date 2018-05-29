Formatfile <- function(File){
  
  ## Separate columns and format them
  File1 <- File
  File1$Datetime <- as.character(File1$Datetime)
  File1 <- mutate(File1, tdate = substr(Datetime, 1,10), thr = substr(Datetime, 12,13))
 ## convert Hour as factor
  File1$thr <- as.factor(File1$thr)
  ## FInd Weekday of the date
  Dt <- as.Date("25-08-2012", "%d-%m-%Y")
  File1 <- mutate(File1, weekday = weekdays(as.Date(File1$tdate, "%d-%m-%Y")), 
                  age = -difftime(Dt, as.Date(File1$tdate, "%d-%m-%Y"), units = "weeks"),
                  Mon = months(as.Date(File1$tdate, "%d-%m-%Y")),
                  Day = substr(File1$tdate,1,2))
  
  File1$weekday <- as.factor(File1$weekday)
  File1$Mon <- factor(File1$Mon)
  File1$Day <- factor(as.character(File1$Day))
  File1$age <- as.numeric(File1$age)
  ## Remove Date coulmns
  File1 <- select(File1, thr, weekday, age, Mon, Day, Count)
  
  ## Return File1
  File1 
}