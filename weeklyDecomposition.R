## What I did to get the 2015 year long graphs.  Note that row 69 had some data that I "created" so that the 
## stl function would run without errors.  Could re-run on other colums too.  

# load
source("~/Documents/Web Analytics/drupFixAnalysis.R")
year2015 <- read.csv("~/Documents/Web Analytics/Data/All_010115to311215.csv", quote="", stringsAsFactors=FALSE)

# clean (need to load drupFixAnalysis.R)
year2015 <- cleanData(year2015)

# deal with NAs
year2015[69, c(2:7,29:38)] <- sapply(year2015[c(68,70), c(2:7,29:38)], mean)


############ you can skip the above if you load all2015.RData #############

# get seasonal fit could repeat this changing colInterest
colInterest <- 3
year.ts = ts(data = year2015[,colInterest], frequency = 7)
yr2 = window(year.ts)
fit = stl( x = yr2, s.window = "periodic")
# to see all four parts
plot(fit)

# to plot just one part against the month it's from...
plot( year2015$Date, year2015[,3], type = "l", main = "Before: Visitors to ebi.ac.uk", xlab = "Date", ylab = "Number of Visits")
plot( year2015$Date, fit$time.series[,2], type = "l", main = "After: Trend of visitors to ebi.ac.uk", xlab = "Date", ylab = "Number of Visits")