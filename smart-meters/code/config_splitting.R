# This script defines the data split into training, validation and test data. 
# It also defines some calendar information for the data.
library(lubridate)
#load(file.path(working.folder, "myinfo.Rdata"))

startTrain <- ymd_hms("2009-4-20 00:00:00"); endTrain <- ymd_hms("2010-3-31 23:30:00")
startVal   <- ymd_hms("2010-4-01 00:00:00"); endVal <- ymd_hms("2010-4-30 23:30:00")
startLearn <- startTrain; 					 endLearn <- endVal
startTest  <- ymd_hms("2010-5-01 00:00:00"); endTest <- ymd_hms("2010-7-31 23:30:00")
startObs   <- startTrain; 					 endObs <- endTest


complete_interval <- lubridate::interval(startObs, endObs)
seq_complete_interval <- seq(int_start(complete_interval), int_end(complete_interval), by = "30 min")

training_interval   <- lubridate::interval(startTrain, endTrain)  # 11 months of training
seq_training_interval <- seq(lubridate::int_start(training_interval), lubridate::int_end(training_interval), by = "30 min")

validation_interval <- lubridate::interval(startVal, endVal) # 1 month of validation
seq_validation_interval <- seq(lubridate::int_start(validation_interval), lubridate::int_end(validation_interval), by = "30 min")

learning_interval   <- lubridate::union(training_interval, validation_interval)
seq_learning_interval <- seq(lubridate::int_start(learning_interval), lubridate::int_end(learning_interval), by = "30 min")

testing_interval     <- lubridate::interval(startTest, endTest)   # 3 months of testing
seq_testing_interval <- seq(lubridate::int_start(testing_interval), lubridate::int_end(testing_interval), by = "30 min")


stopifnot(lubridate::union(learning_interval, testing_interval) == complete_interval)

train <- validation  <- learn <- test  <- NULL
train$id <- match(seq_training_interval, seq_complete_interval)
validation$id <- match(seq_validation_interval, seq_complete_interval)
learn$id <- match(seq_learning_interval, seq_complete_interval)
test$id  <- match(seq_testing_interval, seq_complete_interval)

# Calendar variables for the period
calendar <- NULL
calendar$dweek <- lubridate::wday(seq_complete_interval)
calendar$dweek <- (calendar$dweek - 1) + ((calendar$dweek - 1) == 0)*7

calendar$periodOfDay <- 2*(lubridate::hour(seq_complete_interval) + 1) - (lubridate::minute(seq_complete_interval) == 0)
calendar$tyear <- lubridate::yday(seq_complete_interval)
calendar$periodOfWeek <- (calendar$dweek - 1)*48 + calendar$periodOfDay
calendar$year <- lubridate::year(seq_complete_interval)

periodOfCycle <- calendar$periodOfWeek
periodOfCycle[which(periodOfCycle %in% seq(1, 5*48))] <- 1  
periodOfCycle[which(periodOfCycle %in% seq(5*48 + 1, 6*48))] <- 2
periodOfCycle[which(periodOfCycle %in% seq(6*48 + 1, 7*48))] <- 3  
calendar$periodOfCycle <- periodOfCycle

hol2009England  <- c("2009-1-01", "2009-4-10", "2009-4-13", "2009-5-04", "2009-5-25", "2009-8-31", "2009-12-25", "2009-12-28")
hol2010England  <- c("2010-1-01", "2010-4-2", "2010-4-5", "2010-5-3", "2010-5-31", "2010-8-30", "2010-12-27", "2010-12-28")
holEngland <- ymd(c(hol2009England, hol2010England))

hol2009Scotland <- c("2009-1-01", "2009-1-02", "2009-4-10", "2009-5-04", "2009-5-25",  "2009-8-3", "2009-12-25", "2009-12-28")
hol2010Scotland <- c("2010-1-01", "2010-1-02", "2010-1-03", "2010-1-04", "2010-4-2", "2010-5-3", "2010-5-31", "2010-8-2", "2010-12-27", "2010-12-28")
holScotland     <- ymd(c(hol2009Scotland, hol2010Scotland))

makeHol <- function(holset){
  daysInPeriod <- seq(int_start(complete_interval), int_end(complete_interval), by = "day")
  typeday <- rep("WD", length(daysInPeriod))
  
  id <- na.omit(match(holset, daysInPeriod))
  typeday[id] <- "NWD"
  
  dayOfWeek <- lubridate::wday(daysInPeriod)
  typeday[which(dayOfWeek == 7 | dayOfWeek == 1)] <- "NWD"
  
  hol  <- as.numeric(typeday !="WD" & dayOfWeek != 7 & dayOfWeek != 1)
  hols <- numeric(length(hol))
  
  for(i in 2:(length(hol)-1))
  {
    if(hol[i-1] == 1 & hol[i] == 0)
      hols[i] <- 1
    else if(hol[i+1] == 1 & hol[i] == 0)
      hols[i] <- -1
  }
  hols <- hols + 2*hol
  hols <- factor(hols, levels = c(0,-1,2,1), labels = c("Normal","Day before","Holiday","Day after"))
  return(hols)
}

#calendar$holidayEngland   <- rep(makeHol(holEngland),  each = 48)
#calendar$holidayScotland  <- rep(makeHol(holScotland), each = 48)
myhol <- ymd(intersect(c(hol2009England, hol2010England), c(hol2009Scotland, hol2010Scotland)))
calendar$holiday  <- rep(makeHol(myhol),  each = 48)
