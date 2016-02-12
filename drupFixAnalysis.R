## TO DO
# look into using test.wilcox instead of t.test.  It's more robust

## DONE, Notes
# add in the divsor to correct for multiple comparisions
# add in t.test remember to use data~splitFactor
# add in ks.test as a test of normalicy.  ks.text(data[prePost = TRUE, i], "pnorm") where i is the matrix column name of interest
# use stl to look at % mobile (#mobile/(#mobile+#desktop)) this should split out the "seasonal" week flucations

## NOTES
# The go live date was 26/10/15 and is included in the post-launch segmentation.  
# Analysis was on bounce rate and average actions per visit for RETURNING visitors. 
# t-test that's UNPAIRED
# t.test(preLaunch, postLanuch, paired = FALSE)
# p should be < .05/#tests in order to have significance over multiple 


## To run: 
# source("~/Documents/Web Analytics/DrupFixAnalysis.R")
## load data
# mobileAll <- read.csv("~/Documents/Web Analytics/R Files/Data/MobileAll_010915to301115.csv", quote="", stringsAsFactors=FALSE)
# desktopAll <- read.csv("~/Documents/Web Analytics/R Files/Data/DesktopAll010915to301115.csv", quote="", stringsAsFactors=FALSE)
# analysis(mobileAll, clean = TRUE)
# percentMobile(deskData = desktopAll, mobileData = mobileAll, clean = TRUE)


 

# cleanData takes a piwik file and changes data that "incorrectly" loaded as characters into numerics and dates.  
# The piwik file that this was created (and tested on) was downloaded from Visitors -> Overview then clicking the grey 
# "export arrow" at the bottom left of the graph, downloading a TSV and then saving that from excel into a csv.  
# Note that the piwik csv download seems to have a weird header in it that R can't handle loading.  
cleanData <- function(data) {
  ##convert dates to be of the date format
  #data$Date <- as.Date(data$Date, format = "%d/%m/%y")
 
  ## convert seconds and percents to numeric type from characters
  found <- toFind(data, "rate")
  for (rate in found){
    data[,rate] <- as.numeric(sub("%", "", data[,rate]))/100
  }
  
  ## Convert seconds to numeric type from characters.  USE these columns WITH CARE.
  ## Note: depending on when during the day this code is run, the mean will CHANGE.  The delta between pre/post launch means won't.  
  ## Be careful to use the same sys.time if you're dealing with two data files (would require modifiying code!)
  found<- toFind(data, "avg.*sec")
  for (minSec in found){
    #this assumes the day/time is the same as current day, just changing the time component.  
    sysTime <- Sys.time()
    data[,minSec] <- as.numeric(sysTime - as.POSIXct(data[,minSec], format = "%H:%M:%S"))
    
  }
  
  #remove the s and convert to a numeric.  
  data$Avg..generation.time <- as.numeric(sub("s", "", data$Avg..generation.time))
  
  ## return the cleaned dataset
  data
 
}

## toFind is a helper function for cleanData.  It returns a list of indexes that are the column names with toFind in it.  
toFind <- function(data, toFind = "rate") {
  found <- simplify2array(regexec(toFind, tolower(names(data))))
  which(found > -1)
}

# testForNormality takes one column of numeric data, a true/false vector saying how to segment the column into two populations,
# and an optional p value.  It defaults to .05 if non is given.  
# The function returns true if both populations are normal, false if one of them is not normal and a t-test should NOT be performed.  
testForNormality <- function(colData, prePost, p = .05) {
  # if you want to visulally inspect/plot the data uncomment the next line...
  #hist(colData[prePost])
  #hist(colData[!prePost])
    
  ## test for normality
  test1 <- ks.test(scale(colData[prePost]), "pnorm")
  test2 <- ks.test(scale(colData[!prePost]), "pnorm")
  (test1$p.value > p &&  test2$p.value > p)
}

# analysis takes in a data frame (loaded from piwik engagement cvs file), and tests the columns indexed in dataCol
# to see if the pre and post go-live data is significantly different from each other. (If test.type is "t", data is first checked for normality)
# If clean is TRUE the data is cleaned before analysis (this does not remove NAs). 
# Go-live demarks the date (dd/mm/yy) which segments the two data sets.  

# Piwik data will frequenlty cause warnings during the normality test.  This is "ok" as long as there's not too many duplicate values.  
# Errors can be caused by NAs, and trying to run the test on date variables.  Avoid this.  

# A return value of NA means the populations weren't normal (check for outliers, eg spike caused by news story), 
# True means that the go-live significantly changed those data points
# False means that the go-live did not significantly impact that column of data.  
analysis <- function(data, dataCol = c(14:16,38:41), goLiveDate = "26/10/15", clean = FALSE, test.type = "Wilcox") {
  #if data hasn't already been cleaned, making all columns of interest into numerics, do so now.  
  if(clean) {
      data <- cleanData(data)
  }
    
  #make a true/false vector that will identify pre (false) and post (true) go live data
  prePost <- data$Date > as.Date(goLiveDate, format = "%d/%m/%y")
  
  #set the pass value for p.  To account for multiple tests, it needs to be divided by the number of the tests being run.
  passValue <- .05/length(dataCol)
  
#   for(i in dataCol) {
#     if(checkTest(colData = data[,i], prePost = prePost)>passValue) {
#       warning("Test for ", i, " did not pass")
#     }
#   }
  
  results <- apply(data[,dataCol], MARGIN=2, FUN = checkTest, test.type = test.type, prePost=prePost)
  # true means the means are different, false means they are not significantly different, 
  # NA means it didn't pass the KS test for being normal.
  results < passValue
  
}

#check the colData for normality.  prePost is a logical vector which segments the data into two poplulations needing testing.  
checkTest <- function(colData, prePost, test.type = "t"){
  
  if(test.type == "t") {
    if(testForNormality(colData, prePost = prePost)) {
      t.test(colData~prePost, paired = FALSE)$p.value
    } else{
      NA
    }
  }else {
    wilcox.test(colData~prePost, paired = FALSE)$p.value
  }
}

# Almost exactly like analyse() but takes two data frames which it uses to generate percent mobile data to test.  
percentMobile <- function(deskData, mobileData, colInterest = c(2,3,9,11), goLiveDate = "26/10/15", clean = FALSE){
    # if we need to clean the data, clean away
    if(clean){
        mobileData <- cleanData(mobileData)
        deskData <- cleanData(deskData)
    }
    # turn into a percent mobile/all traffic.  
    percentMobile <- mobileData[,colInterest]/(deskData[,colInterest]+mobileData[,colInterest])
    
    #make a true/false vector that will identify pre (false) and post (true) go live data
    prePost <- mobileData$Date > as.Date(goLiveDate, format = "%d/%m/%y")
    passValue <- .05/length(colInterest)
    
    #for each column of interest, turn into a time series with a window, then extract the remainder.  Check the remainder for normality and  
    testing <- apply(percentMobile, MARGIN = 2, FUN = seasonalTest, prePost = prePost)
    testing < passValue
    
}

# Takes out the seasonal fluctuations before checking for normality and doing the t-test using prePost to
# divide the data into two populations.  
seasonalTest <- function(seasonalData, prePost) {
    # make the seasonalData into a time series object that is periodic over 7 samples.  
    percent.ts = ts(data=seasonalData, frequency = 7)
    per2 = window(percent.ts)
    
    # run the seasonal decomposition of time series by loess
    fit = stl(per2, s.window = "periodic")
    
    # check if the data with the periodic part (aka weekly fluctuations) removed is normal and signficantly different
    # column 2 is trend and 3 is remainder (1 is the periodic data) seasonalData = col1+col2+col3
    checkTest(fit$time.series[,2]+fit$time.series[,3], prePost)
}


