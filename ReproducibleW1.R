ReproducibleW1 <- function() {
    #This function opens the activity data and plots the required graphs

    #Load the data
    dat<-loadData("activity.csv")
    #Put all graphs on the same plot next to each other
    par(mfcol = c(2, 2)) 
    #Run the analysis
    newDat<-run_analysis(dat)
    #Return the modified data for further exploratory analysis
    newDat
}

loadData <- function(loadName = "activity.csv") {
    #This function loads the data and converts the interval column into minutes after midnight
    
    ##load the data
    dat <- read.csv(loadName)
    #Convert the interval column to minuts after midnight
    dat$interval<-with(dat,as.double(rep(as.double(seq(unique(interval)))*5,length(interval)/length(unique(interval)))))
    #Return the data
    dat
    
}

run_analysis <- function(dat) {
    #This function calls the other functions in sequence to fulfill the required analysis
    
    #Average steps pr day
    calc_StepsDay(dat)
    #The daily activity
    calc_DailyPattern(dat)
    #Fill in missing data
    newDat<-calc_missingValues(dat)
    #Calculate the new average steps pr day
    calc_StepsDay(newDat)
    #Calculate the the daily activity in the weekdays and weekends
    calc_weekdays(newDat)
    #return the modified data
    newDat
}

calc_StepsDay <- function(dat) {
    #This function calculates the average steps pr day. The data is plotted as a histogram and the mean and median
    #values are written out
    
    #Separate the data according to the date and calculate the sum
    stepsDay<-tapply(dat$steps, dat$date, function(x) sum(x, na.rm=TRUE))
    #Plot a histogram of the steps pr day
    hist(stepsDay,
         main="Histogram of steps/day",
         xlab="steps/day")
    #Print out the median and mean steps pr day
    print("Mean number of steps taken pr day")
    print(as.integer(round(mean(stepsDay),0)))
    print("Median number of steps taken pr day")
    print(as.integer(median(stepsDay)))
}

calc_DailyPattern <- function(dat) {
    #This function calculates the daily activity pattern as the average number of steps in 5-minute intervals.
    #The data is plotted as time of day vs. the number of step and the time of highest activity is printed out.
    
    #Separates the data according to the time of day and calculates the average.
    steps5MinInterval<-tapply(dat$steps, dat$interval, function(x) mean(x, na.rm=TRUE))
    #Plot the data as time of day vs. the number of steps
    plot(as.double(rownames(steps5MinInterval))/60,steps5MinInterval, 
         type="l",
         xlab = "Time of day (hours)",
         ylab = "Average number of steps in 5 min intervals",
         main = "Daily activity pattern")
    #Find and print out the time of day with highest activity
    print("Time of highest activity")
    stepMax5MinInterval <- as.double(rownames(steps5MinInterval))[which.max(steps5MinInterval)]
    print(paste0(floor(stepMax5MinInterval/60),":",stepMax5MinInterval %% 60))
    
}

calc_missingValues <- function(dat) {
    #This function finds and fills in missing data points. The missing points are filled in by the average
    #on the same time on other days.
    
    #which values are missing? Print out the amount (as both specific and relative)
    missingValues <- is.na(dat$steps)
    print("The amount of missing data points")
    print(paste0(sum(missingValues)," (",round(sum(missingValues)/length(dat$steps)*100,0),"%)"))
    #Calculates the mean steps on all days at the specific times.
    meanSteps<-tapply(dat$steps, dat$interval, function(x) mean(x, na.rm=TRUE))
    #Substitutes the missing values with the mean
    dat$steps[missingValues]<-rep(meanSteps,length(dat$steps)/length(meanSteps))[missingValues]
    print("Missing data has been substituted with the average steps for that interval on other days")
    #Return the modified data
    dat
}

calc_weekdays <- function(dat) {
    #This function adds a new factor with two layers that specifies whether the day is a weekday or weekend.
    #This new factor is used to seperate the activity pattern.
    #The data is plotted as in calc_DailyPattern.
    
    #The new weekday/weekend factor is calculated from the data variable.
    isWeekend<-factor(floor(as.integer(strftime(as.Date(dat$date),"%u"))/6), labels = c("Weekday","Weekend"))
    #Add the factor to the data frame
    dat<-cbind(dat,isWeekend)
    #Calculate the daily activity as in calc_Dailypattern but with the data separated by the new factor
    steps5MinInterval<-data.frame(tapply(dat$steps, dat[,3:4], function(x) mean(x, na.rm=TRUE)))
    #Plot the new data with weekday in red and weekend in blue.
    plot(as.double(rownames(steps5MinInterval))/60,steps5MinInterval$Weekday, col="red",
         type="l",
         xlab = "Time of day (hours)",
         ylab = "Average number of steps in 5 min intervals",
         main = "Daily activity pattern")
    lines(as.double(rownames(steps5MinInterval))/60,steps5MinInterval$Weekend, col="blue")
    legend(17,220, c("Weekday","Weekend"), lty=c(1,1), col=c("red","blue"), lwd=c(2.5,2.5),)
    
}