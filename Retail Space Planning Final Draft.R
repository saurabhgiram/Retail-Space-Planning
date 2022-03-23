
# Import libraries ------------------------------------------------------


## Install packages --------------------------------------------------------


install.packages('ggplot2') # visualisation
install.packages('scales') # visualisation
install.packages('patchwork') # visualisation
install.packages('RColorBrewer') # visualisation
install.packages('corrplot') # visualisation

install.packages('readr') # input/output
install.packages('vroom') # input/output
install.packages('skimr') # overview
install.packages('tibble') # data wrangling
install.packages('tidyr') # data wrangling
install.packages('purrr') # data wrangling
install.packages('stringr') # string manipulation
install.packages('forcats') # factor manipulation
install.packages('fuzzyjoin') # data wrangling
install.packages('alluvial') # visualisation
install.packages('ggrepel') # visualisation
install.packages('ggforce') # visualisation
install.packages('ggridges') # visualisation
install.packages('gganimate') # animations
install.packages('GGally') # visualisation
install.packages('ggthemes') # visualisation
install.packages('wesanderson') # visualisation
install.packages('kableExtra') # display
install.packages('lubridate') # date and time
install.packages('forecast') # time series analysis
install.packages('timetk') # time series analysis
install.packages('crosstalk')
install.packages('plotly')
install.packages("foreach")
install.packages("doParallel")
install.packages("rlang")

install.packages('dplyr') # data manipulation
install.packages("rlang")
install.packages("xts")
install.packages("forecast")
install.packages("tsutils")
install.packages("imputES")
install.packages("tseries")
install.packages("readxl")
install.packages("xts")
install.packages("seastests")
install.packages("tinytex")
install.packages("tsibble")
install.packages("dplyr")
install.packages("outliers")
install.packages("moments")
install.packages("VIM")
install.packages("naniar")
install.packages("ggplot2")
install.packages("imputeTS")
install.packages("knitr")
install.packages("regclass")

## Import Libraries --------------------------------------------------------

# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('patchwork') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('vroom') # input/output
library('skimr') # overview
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('purrr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('fuzzyjoin') # data wrangling
# specific visualisation
library('alluvial') # visualisation
library('ggrepel') # visualisation
library('ggforce') # visualisation
library('ggridges') # visualisation
library('gganimate') # animations
library('GGally') # visualisation
library('ggthemes') # visualisation
library('wesanderson') # visualisation
library('kableExtra') # display
# Date + forecast
library('lubridate') # date and time
library('forecast') # time series analysis
library('timetk') # time series analysis
# Interactivity
library('crosstalk')
library('plotly')
# parallel
library('foreach')
library('doParallel')
library('tseries')
library('xts')
library('SWMPr')
library("forecast")
library("tsutils")
library("imputES")
library("tseries")
library("readxl")
library("xts")
library("seastests")
library("tinytex")
library("tsibble")
library("dplyr")
library("outliers")
library("moments")
library("VIM")
library("naniar")
library("ggplot2")
library("imputeTS")
library("knitr")
library("regclass")

# Load the dataset --------------------------------------------------------

#dataset <- vroom(str_c('sales_train_evaluation.csv'), delim = ",", col_types = cols())
dataset <- read.csv("sales_train_evaluation.csv", stringsAsFactors = TRUE)
#View(dataset)
calendar <- read.csv("calendar.csv", stringsAsFactors = TRUE)


# Aggregate the dataset ---------------------------------------------------

# By State-wise
grpbystate <- dataset %>%group_by(state_id) %>% summarise_at(vars(starts_with("d_")), sum) %>% rename(id = state_id)
#View(grpbystate)
rownames(grpbystate)
colnames(grpbystate)
grpbystate <- data.frame(grpbystate, row.names = 1)

# By Store-wise
grpbystore <- dataset %>%group_by(store_id) %>% summarise_at(vars(starts_with("d_")), sum) %>% rename(id = store_id)
#View(grpbystore)
grpbystore <- data.frame(grpbystore, row.names = 1)
#View(grpbystore)

# By Category-wise
grpbycat <- dataset %>%group_by(cat_id) %>% summarise_at(vars(starts_with("d_")), sum) %>% rename(id = cat_id)
#View(grpbycat)
grpbycat <- data.frame(grpbycat, row.names = 1)
#View(grpbycat)

# By Department-wise
grpbydept <- dataset %>%group_by(dept_id) %>% summarise_at(vars(starts_with("d_")), sum) %>% rename(id = dept_id)
#View(grpbydept)
grpbydept <- data.frame(grpbydept, row.names = 1)
#View(grpbydept)

# Combine all the aggregated dataset into one
combine <- rbind(grpbydept, grpbycat)
combine <- rbind(combine, grpbystore)
combine <- rbind(combine, grpbystate)
#View(combine)
rownames(combine)
colnames(combine)

# Transpose the combine dataset
transpose <- t(combine)
#View(transpose)
row.names(transpose)
colnames(transpose)
dim(transpose)
class(transpose)
transpose <- data.frame(transpose)

# Remove the last 28 entries from the calender csv so that we can merge calender and transposed dataset into one
calendar <- head(calendar, - 28)
#View(calendar)

# Merge the calender and transposed dataset
total <- cbind(transpose,calendar)
#View(total)
colnames(total)
rownames(total)
total <- data.frame(total)
str(total)
summary(total)
walmart_weekly <- ts(total, frequency = 52, start=c(2011,4))
walmart_weekly

walmart_monthly <- ts(total, frequency = 12, start=c(2011,1))
walmart_monthly

# Create monthly and weekly data ------------------------------------------

dates <- seq(as.Date("2011-01-29"), length = 1941, by = "days")
data <- total[,c(1:23)]

xts_lxy <- xts(x=data,order.by = dates)
#View(xts_lxy)

monthly_mean <- apply.monthly(xts_lxy,mean)
#View(monthly_mean)
head(monthly_mean)
summary(weekly_mean)

weekly_mean <- apply.weekly(xts_lxy,mean)
#View(weekly_mean)
head(weekly_mean)

forecast <- read

yearly_mean <- apply.yearly(xts_lxy,mean)
#View(yearly_mean)

sat_mean_FOODS_1 <-  mean(total[total$weekday == 'Saturday', 'FOODS_1'])
sun_mean_FOODS_1 <-  mean(total[total$weekday == 'Sunday', 'FOODS_1'])
mon_mean_FOODS_1 <-  mean(total[total$weekday == 'Monday', 'FOODS_1'])
tue_mean_FOODS_1 <-  mean(total[total$weekday == 'Tuesday', 'FOODS_1'])
wed_mean_FOODS_1 <-  mean(total[total$weekday == 'Wednesday', 'FOODS_1'])
thu_mean_FOODS_1 <-  mean(total[total$weekday == 'Thursday', 'FOODS_1'])
fri_mean_FOODS_1 <-  mean(total[total$weekday == 'Friday', 'FOODS_1'])

sat_mean_FOODS_1
sun_mean_FOODS_1
mon_mean_FOODS_1
tue_mean_FOODS_1
wed_mean_FOODS_1
thu_mean_FOODS_1
fri_mean_FOODS_1

sat_mean_FOODS_2 <-  mean(total[total$weekday == 'Saturday', 'FOODS_2'])
sun_mean_FOODS_2 <-  mean(total[total$weekday == 'Sunday', 'FOODS_2'])
mon_mean_FOODS_2 <-  mean(total[total$weekday == 'Monday', 'FOODS_2'])
tue_mean_FOODS_2 <-  mean(total[total$weekday == 'Tuesday', 'FOODS_2'])
wed_mean_FOODS_2 <-  mean(total[total$weekday == 'Wednesday', 'FOODS_2'])
thu_mean_FOODS_2 <-  mean(total[total$weekday == 'Thursday', 'FOODS_2'])
fri_mean_FOODS_2 <-  mean(total[total$weekday == 'Friday', 'FOODS_2'])

sat_mean_FOODS_2
sun_mean_FOODS_2
mon_mean_FOODS_2
tue_mean_FOODS_2
wed_mean_FOODS_2
thu_mean_FOODS_2
fri_mean_FOODS_2


?mean()
  
# Plot the monthly and weekly dataset -------------------------------------

par(mfrow=c(2,2))
ggseasonplot(ts(monthly_mean,frequency = 12)[,1],year.labels=TRUE)
ggseasonplot(ts(monthly_mean,frequency = 12)[,2],year.labels=TRUE)
ggseasonplot(ts(monthly_mean,frequency = 12)[,3],year.labels=TRUE)
ggseasonplot(ts(monthly_mean,frequency = 12)[,4],year.labels=TRUE)
ggseasonplot(ts(monthly_mean,frequency = 12)[,5],year.labels=TRUE)
ggseasonplot(ts(monthly_mean,frequency = 12)[,6],year.labels=TRUE)
ggseasonplot(ts(monthly_mean,frequency = 12)[,7],year.labels=TRUE)

# Convert the dataset into time series


## Convert the dataset into weekly time series -----------------------------


walmart_weekly_FOODS_1 <- ts(weekly_mean$FOODS_1, frequency = 7, start=c(2011,29))
walmart_weekly_FOODS_2 <- ts(weekly_mean$FOODS_2, frequency = 52, start=c(2011,4))
walmart_weekly_FOODS_3 <- ts(weekly_mean$FOODS_3, frequency = 52, start=c(2011,4))
walmart_weekly_HOBBIES_1 <- ts(weekly_mean$HOBBIES_1, frequency = 52, start=c(2011,4))
walmart_weekly_HOBBIES_2 <- ts(weekly_mean$HOBBIES_2, frequency = 52, start=c(2011,4))
walmart_weekly_HOUSEHOLD_1 <- ts(weekly_mean$HOUSEHOLD_1, frequency = 52, start=c(2011,4))
walmart_weekly_HOUSEHOLD_2 <- ts(weekly_mean$HOUSEHOLD_2, frequency = 52, start=c(2011,4))
walmart_weekly_FOODS <- ts(weekly_mean$FOODS, frequency = 52, start=c(2011,4))
walmart_weekly_HOBBIES <- ts(weekly_mean$HOBBIES, frequency = 52, start=c(2011,4))
walmart_weekly_HOUSEHOLD <- ts(weekly_mean$HOUSEHOLD, frequency = 52, start=c(2011,4))
walmart_weekly_CA <- ts(weekly_mean$CA, frequency = 52, start=c(2011,4))
walmart_weekly_TX <- ts(weekly_mean$TX, frequency = 52, start=c(2011,4))
walmart_weekly_WI <- ts(weekly_mean$WI, frequency = 52, start=c(2011,4))

ggseasonplot(walmart_weekly_FOODS_1)

## Convert the dataset into monthly time series ----------------------------



walmart_monthly <- ts(monthly_mean, frequency = 12)
walmart_monthly_FOODS_1 <- ts(monthly_mean$FOODS_1, frequency = 12, start=c(2011,1))
walmart_monthly_FOODS_2 <- ts(monthly_mean$FOODS_2, frequency = 12, start=c(2011,1))
walmart_monthly_FOODS_3 <- ts(monthly_mean$FOODS_3, frequency = 12, start=c(2011,1))
walmart_monthly_HOBBIES_1 <- ts(monthly_mean$HOBBIES_1, frequency = 12, start=c(2011,1))
walmart_monthly_HOBBIES_2 <- ts(monthly_mean$HOBBIES_2, frequency = 12, start=c(2011,1))
walmart_monthly_HOUSEHOLD_1 <- ts(monthly_mean$HOUSEHOLD_1, frequency = 12, start=c(2011,1))
walmart_monthly_HOUSEHOLD_2 <- ts(monthly_mean$HOUSEHOLD_2, frequency = 12, start=c(2011,1))
walmart_monthly_FOODS <- ts(monthly_mean$FOODS, frequency = 12, start=c(2011,1))
walmart_monthly_HOBBIES <- ts(monthly_mean$HOBBIES, frequency = 12, start=c(2011,1))
walmart_monthly_HOUSEHOLD <- ts(monthly_mean$HOUSEHOLD, frequency = 12, start=c(2011,1))
walmart_monthly_CA <- ts(monthly_mean$CA, frequency = 12, start=c(2011,1))
walmart_monthly_TX <- ts(monthly_mean$TX, frequency = 12, start=c(2011,1))
walmart_monthly_WI <- ts(monthly_mean$WI, frequency = 12, start=c(2011,1))

## Convert the dataset into yearly time series ----------------------------
walmart_yearly <- ts(yearly_mean, frequency = 1, start=c(2011))
walmart_yearly_FOODS_1 <- ts(yearly_mean$FOODS_1, frequency = 1, start=c(2011))
walmart_yearly_FOODS_2 <- ts(yearly_mean$FOODS_2, frequency = 1, start=c(2011))
walmart_yearly_FOODS_3 <- ts(yearly_mean$FOODS_3, frequency = 1, start=c(2011))
walmart_yearly_HOBBIES_1 <- ts(yearly_mean$HOBBIES_1, frequency = 1, start=c(2011))
walmart_yearly_HOBBIES_2 <- ts(yearly_mean$HOBBIES_2, frequency = 1, start=c(2011))
walmart_yearly_HOUSEHOLD_1 <- ts(yearly_mean$HOUSEHOLD_1, frequency = 1, start=c(2011))
walmart_yearly_HOUSEHOLD_2 <- ts(yearly_mean$HOUSEHOLD_2, frequency = 1, start=c(2011))
walmart_yearly_FOODS <- ts(yearly_mean$FOODS, frequency = 1, start=c(2011))
walmart_yearly_HOBBIES <- ts(yearly_mean$HOBBIES, frequency = 1, start=c(2011))
walmart_yearly_HOUSEHOLD <- ts(yearly_mean$HOUSEHOLD, frequency = 1, start=c(2011))
walmart_yearly_CA <- ts(yearly_mean$CA, frequency = 1, start=c(2011))
walmart_yearly_TX <- ts(yearly_mean$TX, frequency = 1, start=c(2011))
walmart_yearly_WI <- ts(yearly_mean$WI, frequency = 1, start=c(2011))


# Create training and testing dataset -------------------------------------

## Train and test dataset for weekly series --------------------------------

train_weekly_FOODS_1 <- ts(walmart_weekly_FOODS_1[c(1:195),],frequency = 52)
test_weekly_FOODS_1 <- (walmart_weekly_FOODS_1[c(196:278),])
train_weekly_FOODS_2 <- ts(walmart_weekly_FOODS_2[c(1:195),],frequency = 52)
test_weekly_FOODS_2 <- (walmart_weekly_FOODS_2[c(196:278),])
train_weekly_FOODS_3 <- ts(walmart_weekly_FOODS_3[c(1:195),],frequency = 52)
test_weekly_FOODS_3 <- (walmart_weekly_FOODS_3[c(196:278),])
train_weekly_HOBBIES_1 <- ts(walmart_weekly_HOBBIES_1[c(1:195),],frequency = 52)
test_weekly_HOBBIES_1 <- (walmart_weekly_HOBBIES_1[c(196:278),])
train_weekly_HOBBIES_2 <- ts(walmart_weekly_HOBBIES_2[c(1:195),],frequency = 52)
test_weekly_HOBBIES_2 <- (walmart_weekly_HOBBIES_2[c(196:278),])
train_weekly_HOUSEHOLD_1<- ts(walmart_weekly_HOUSEHOLD_1[c(1:195),],frequency = 52)
test_weekly_HOUSEHOLD_1 <- (walmart_weekly_HOUSEHOLD_1[c(196:278),])
train_weekly_HOUSEHOLD_2 <- ts(walmart_weekly_HOUSEHOLD_2[c(1:195),],frequency = 52)
test_weekly_HOUSEHOLD_2 <- (walmart_weekly_HOUSEHOLD_2[c(196:278),])
train_weekly_FOODS <- ts(walmart_weekly_FOODS[c(1:195),],frequency = 52)
test_weekly_FOODS <- (walmart_weekly_FOODS[c(196:278),])
train_weekly_HOBBIES <- ts(walmart_weekly_HOBBIES[c(1:195),],frequency = 52)
test_weekly_HOBBIES <- (walmart_weekly_HOBBIES[c(196:278),])
train_weekly_HOUSEHOLD <- ts(walmart_weekly_HOUSEHOLD[c(1:195),],frequency = 52)
test_weekly_HOUSEHOLD <- (walmart_weekly_HOUSEHOLD[c(196:278),])
train_weekly_CA <- ts(walmart_weekly_CA[c(1:195),],frequency = 52)
test_weekly_CA <- (walmart_weekly_CA[c(196:278),])
train_weekly_TX <- ts(walmart_weekly_TX[c(1:195),],frequency = 52)
test_weekly_TX <- (walmart_weekly_TX[c(196:278),])
train_weekly_WI <- ts(walmart_weekly_WI[c(1:195),],frequency = 52)
test_weekly_WI <- (walmart_weekly_WI[c(196:278),])

## Train and test dataset for monthly series -------------------------------

train_monthly_FOODS_1 <- ts(walmart_monthly_FOODS_1[c(1:45),],frequency = 12)
test_monthly_FOODS_1 <- (walmart_monthly_FOODS_1[c(46:65),])
train_monthly_FOODS_2 <- ts(walmart_monthly_FOODS_2[c(1:45),],frequency = 12)
test_monthly_FOODS_2 <- (walmart_monthly_FOODS_2[c(46:65),])
train_monthly_FOODS_3 <- ts(walmart_monthly_FOODS_3[c(1:45),],frequency = 12)
test_monthly_FOODS_3 <- (walmart_monthly_FOODS_3[c(46:65),])
train_monthly_HOBBIES_1 <- ts(walmart_monthly_HOBBIES_1[c(1:45),],frequency = 12)
test_monthly_HOBBIES_1 <- (walmart_monthly_HOBBIES_1[c(46:65),])
train_monthly_HOBBIES_2 <- ts(walmart_monthly_HOBBIES_2[c(1:45),],frequency = 12)
test_monthly_HOBBIES_2 <- (walmart_monthly_HOBBIES_2[c(46:65),])
train_monthly_HOUSEHOLD_1<- ts(walmart_monthly_HOUSEHOLD_1[c(1:45),],frequency = 12)
test_monthly_HOUSEHOLD_1 <- (walmart_monthly_HOUSEHOLD_1[c(46:65),])
train_monthly_HOUSEHOLD_2 <- ts(walmart_monthly_HOUSEHOLD_2[c(1:45),],frequency = 12)
test_monthly_HOUSEHOLD_2 <- (walmart_monthly_HOUSEHOLD_2[c(46:65),])
train_monthly_FOODS <- ts(walmart_monthly_FOODS[c(1:45),],frequency = 12)
test_monthly_FOODS <- (walmart_monthly_FOODS[c(46:65),])
train_monthly_HOBBIES <- ts(walmart_monthly_HOBBIES[c(1:45),],frequency = 12)
test_monthly_HOBBIES <- (walmart_monthly_HOBBIES[c(46:65),])
train_monthly_HOUSEHOLD <- ts(walmart_monthly_HOUSEHOLD[c(1:45),],frequency = 12)
test_monthly_HOUSEHOLD <- (walmart_monthly_HOUSEHOLD[c(46:65),])
train_monthly_CA <- ts(walmart_monthly_CA[c(1:45),],frequency = 12)
test_monthly_CA <- (walmart_monthly_CA[c(46:65),])
train_monthly_TX <- ts(walmart_monthly_TX[c(1:45),],frequency = 12)
test_monthly_TX <- (walmart_monthly_TX[c(46:65),])
train_monthly_WI <- ts(walmart_monthly_WI[c(1:45),],frequency = 12)
test_monthly_WI <- (walmart_monthly_WI[c(46:65),])

# Set the horizon
week_h <- 83
month_h <- 20

weekshort_h <- 26.0715
monthshort_h <- 6

weeklong_h <- 1043.57
monthlong_h <- 240.64

# Summary of dataset
summary(walmart_weekly)
summary(walmart_monthly)
summary(walmart_yearly)

# Box plot the outliers
boxplot(walmart_weekly)
boxplot(walmart_monthly)
boxplot(walmart_yearly)

# Check how many are the outliers
boxplot.stats(walmart_weekly)
boxplot.stats(walmart_monthly)
boxplot.stats(walmart_yearly)

# Frequency of dataset 
frequency(walmart_weekly)
frequency(walmart_monthly)
frequency(walmart_yearly)

forecast <- read.csv("forecast.csv")
forecast

# Decomposition of the dataset using decompose function -------------------

decomp_weekly_FOODS_1<- decompose(walmart_weekly_FOODS_1)
decomp_weekly_FOODS_2<- decompose(walmart_weekly_FOODS_2)
decomp_weekly_FOODS_3<- decompose(walmart_weekly_FOODS_3)
decomp_weekly_HOBBIES_1 <- decompose(walmart_weekly_HOBBIES_1)
decomp_weekly_HOBBIES_2 <- decompose(walmart_weekly_HOBBIES_2)
decomp_weekly_HOUSEHOLD_1 <- decompose(walmart_weekly_HOUSEHOLD_1)
decomp_weekly_HOUSEHOLD_2<- decompose(walmart_weekly_HOUSEHOLD_2)
decomp_weekly_FOODS<- decompose(walmart_weekly_FOODS)
decomp_weekly_HOBBIES <- decompose(walmart_weekly_HOBBIES)
decomp_weekly_HOUSEHOLD<- decompose(walmart_weekly_HOUSEHOLD)
decomp_weekly_CA<- decompose(walmart_weekly_CA)
decomp_weekly_TX<- decompose(walmart_weekly_TX)
decomp_weekly_WI<- decompose(walmart_weekly_WI)

plot(decomp_weekly_FOODS_1)
plot(decomp_weekly_FOODS_2)
plot(decomp_weekly_FOODS_3)
plot (decomp_weekly_HOBBIES_1)
plot (decomp_weekly_HOBBIES_2)
plot (decomp_weekly_HOUSEHOLD_1)
plot (decomp_weekly_HOUSEHOLD_2) 
plot (decomp_weekly_FOODS)
plot (decomp_weekly_HOBBIES)
plot (decomp_weekly_HOUSEHOLD) 
plot (decomp_weekly_CA)
plot (decomp_weekly_TX) 
plot (decomp_weekly_WI) 


ggseasonplot(daily_ts)

# Plot the season
seasplot(walmart_weekly_FOODS_1,m = 52)
seasplot(walmart_weekly_FOODS_2,m = 52)
seasplot(walmart_weekly_FOODS_3,m = 52)
seasplot(walmart_weekly_HOBBIES_1,m = 52)
seasplot(walmart_weekly_HOBBIES_2,m = 52)
seasplot(walmart_weekly_HOUSEHOLD_1,m = 52)
seasplot(walmart_weekly_HOUSEHOLD_2,m = 52)
seasplot(walmart_weekly_FOODS,m = 52)
seasplot(walmart_weekly_HOBBIES,m = 52)
seasplot(walmart_weekly_HOUSEHOLD,m = 52)
seasplot(walmart_weekly_CA,m = 52)
seasplot(walmart_weekly_TX,m = 52)
seasplot(walmart_weekly_WI,m = 52)


#ACF and PACF analysis on the entire dataset


## ACF and PACF analysis on the weekly dataset -----------------------------

tsdisplay(walmart_weekly_FOODS_1)
tsdisplay(walmart_weekly_FOODS_2)
tsdisplay(walmart_weekly_FOODS_3)
tsdisplay(walmart_weekly_HOBBIES_1)
tsdisplay(walmart_weekly_HOBBIES_2)
tsdisplay(walmart_weekly_HOUSEHOLD_1)
tsdisplay(walmart_weekly_HOUSEHOLD_2)
tsdisplay(walmart_weekly_FOODS)
tsdisplay(walmart_weekly_HOBBIES)
tsdisplay(walmart_weekly_HOUSEHOLD)
tsdisplay(walmart_weekly_CA)
tsdisplay(walmart_weekly_TX)
tsdisplay(walmart_weekly_WI)

## ACF and PACF analysis on the monthly dataset -----------------------------

tsdisplay(walmart_monthly_FOODS_1)
tsdisplay(walmart_monthly_FOODS_2)
tsdisplay(walmart_monthly_FOODS_3)
tsdisplay(walmart_monthly_HOBBIES_1)
tsdisplay(walmart_monthly_HOBBIES_2)
tsdisplay(walmart_monthly_HOUSEHOLD_1)
tsdisplay(walmart_monthly_HOUSEHOLD_2)
tsdisplay(walmart_monthly_FOODS)
tsdisplay(walmart_monthly_HOBBIES)
tsdisplay(walmart_monthly_HOUSEHOLD)
tsdisplay(walmart_monthly_CA)
tsdisplay(walmart_monthly_TX)
tsdisplay(walmart_monthly_WI)

# Perform KPSS and ADF test on time series

## Perform KPSS and ADF test on weekly time series -----------------------------
kpss.test(walmart_weekly_FOODS_1)
kpss.test(walmart_weekly_FOODS_2)
kpss.test(walmart_weekly_FOODS_3)
kpss.test(walmart_weekly_HOBBIES_1)
kpss.test(walmart_weekly_HOBBIES_2)
kpss.test(walmart_weekly_HOUSEHOLD_1)
kpss.test(walmart_weekly_HOUSEHOLD_2)
kpss.test(walmart_weekly_FOODS)
kpss.test(walmart_weekly_HOBBIES)
kpss.test(walmart_weekly_HOUSEHOLD)
kpss.test(walmart_weekly_CA)
kpss.test(walmart_weekly_TX)
kpss.test(walmart_weekly_WI)

adf.test(walmart_weekly_FOODS_1)
adf.test(walmart_weekly_FOODS_2)
adf.test(walmart_weekly_FOODS_3)
adf.test(walmart_weekly_HOBBIES_1)
adf.test(walmart_weekly_HOBBIES_2)
adf.test(walmart_weekly_HOUSEHOLD_1)
adf.test(walmart_weekly_HOUSEHOLD_2)
adf.test(walmart_weekly_FOODS)
adf.test(walmart_weekly_HOBBIES)
adf.test(walmart_weekly_HOUSEHOLD)
adf.test(walmart_weekly_CA)
adf.test(walmart_weekly_TX)
adf.test(walmart_weekly_WI)

## Perform KPSS and ADF test on monthly time series -----------------------------

kpss.test(walmart_monthly_FOODS_1)
kpss.test(walmart_monthly_FOODS_2)
kpss.test(walmart_monthly_FOODS_3)
kpss.test(walmart_monthly_HOBBIES_1)
kpss.test(walmart_monthly_HOBBIES_2)
kpss.test(walmart_monthly_HOUSEHOLD_1)
kpss.test(walmart_monthly_HOUSEHOLD_2)
kpss.test(walmart_monthly_FOODS)
kpss.test(walmart_monthly_HOBBIES)
kpss.test(walmart_monthly_HOUSEHOLD)
kpss.test(walmart_monthly_CA)
kpss.test(walmart_monthly_TX)
kpss.test(walmart_monthly_WI)

adf.test(walmart_monthly_FOODS_1)
adf.test(walmart_monthly_FOODS_2)
adf.test(walmart_monthly_FOODS_3)
adf.test(walmart_monthly_HOBBIES_1)
adf.test(walmart_monthly_HOBBIES_2)
adf.test(walmart_monthly_HOUSEHOLD_1)
adf.test(walmart_monthly_HOUSEHOLD_2)
adf.test(walmart_monthly_FOODS)
adf.test(walmart_monthly_HOBBIES)
adf.test(walmart_monthly_HOUSEHOLD)
adf.test(walmart_monthly_CA)
adf.test(walmart_monthly_TX)
adf.test(walmart_monthly_WI)

# Seasonal Naive Model for Weekly Series ----------------------------------

fc_snaive_walmart_weekly_FOODS_1 <- snaive(train_weekly_FOODS_1, h=week_h)$mean
accuracy(fc_snaive_walmart_weekly_FOODS_1, test_weekly_FOODS_1)

fc_snaive_walmart_weekly_FOODS_2 <- snaive(train_weekly_FOODS_2, h=week_h)$mean
accuracy(fc_snaive_walmart_weekly_FOODS_2, test_weekly_FOODS_2)

fc_snaive_walmart_weekly_FOODS_3 <- snaive(train_weekly_FOODS_3, h=week_h)$mean
accuracy(fc_snaive_walmart_weekly_FOODS_3, test_weekly_FOODS_3)

fc_snaive_walmart_weekly_HOBBIES_1 <- snaive(train_weekly_HOBBIES_1, h=week_h)$mean
accuracy(fc_snaive_walmart_weekly_HOBBIES_1, test_weekly_HOBBIES_1)

fc_snaive_walmart_weekly_HOBBIES_2 <- snaive(train_weekly_HOBBIES_2, h=week_h)$mean
accuracy(fc_snaive_walmart_weekly_HOBBIES_2, test_weekly_HOBBIES_2)

fc_snaive_walmart_weekly_HOUSEHOLD_1 <- snaive(train_weekly_HOUSEHOLD_1, h=week_h)$mean
accuracy(fc_snaive_walmart_weekly_HOUSEHOLD_1, test_weekly_HOUSEHOLD_1)

fc_snaive_walmart_weekly_HOUSEHOLD_2 <- snaive(train_weekly_HOUSEHOLD_2, h=week_h)$mean
accuracy(fc_snaive_walmart_weekly_HOUSEHOLD_2, test_weekly_HOUSEHOLD_2)

fc_snaive_walmart_weekly_FOODS <- snaive(train_weekly_FOODS, h=week_h)$mean
accuracy(fc_snaive_walmart_weekly_FOODS, test_weekly_FOODS)

fc_snaive_walmart_weekly_HOBBIES <- snaive(train_weekly_HOBBIES, h=week_h)$mean
accuracy(fc_snaive_walmart_weekly_HOBBIES, test_weekly_HOBBIES)

fc_snaive_walmart_weekly_HOUSEHOLD <- snaive(train_weekly_HOUSEHOLD, h=week_h)$mean
accuracy(fc_snaive_walmart_weekly_HOUSEHOLD, test_weekly_HOUSEHOLD)

fc_snaive_walmart_weekly_CA <- snaive(train_weekly_CA, h=week_h)$mean
accuracy(fc_snaive_walmart_weekly_CA, test_weekly_CA)

fc_snaive_walmart_weekly_TX <- snaive(train_weekly_TX, h=week_h)$mean
accuracy(fc_snaive_walmart_weekly_TX, test_weekly_TX)

fc_snaive_walmart_weekly_WI <- snaive(train_weekly_WI, h=week_h)$mean
accuracy(fc_snaive_walmart_weekly_WI, test_weekly_WI)


# Seasonal Naive Model for Monthly Series ----------------------------------

fc_snaive_walmart_monthly_FOODS_1 <- snaive(train_monthly_FOODS_1, h=month_h)$mean
accuracy(fc_snaive_walmart_monthly_FOODS_1, test_monthly_FOODS_1)

fc_snaive_walmart_monthly_FOODS_2 <- snaive(train_monthly_FOODS_2, h=month_h)$mean
accuracy(fc_snaive_walmart_monthly_FOODS_2, test_monthly_FOODS_2)

fc_snaive_walmart_monthly_FOODS_3 <- snaive(train_monthly_FOODS_3, h=month_h)$mean
accuracy(fc_snaive_walmart_monthly_FOODS_3, test_monthly_FOODS_3)

fc_snaive_walmart_monthly_HOBBIES_1 <- snaive(train_monthly_HOBBIES_1, h=month_h)$mean
accuracy(fc_snaive_walmart_monthly_HOBBIES_1, test_monthly_HOBBIES_1)

fc_snaive_walmart_monthly_HOBBIES_2 <- snaive(train_monthly_HOBBIES_2, h=month_h)$mean
accuracy(fc_snaive_walmart_monthly_HOBBIES_2, test_monthly_HOBBIES_2)

fc_snaive_walmart_monthly_HOUSEHOLD_1 <- snaive(train_monthly_HOUSEHOLD_1, h=month_h)$mean
accuracy(fc_snaive_walmart_monthly_HOUSEHOLD_1, test_monthly_HOUSEHOLD_1)

fc_snaive_walmart_monthly_HOUSEHOLD_2 <- snaive(train_monthly_HOUSEHOLD_2, h=month_h)$mean
accuracy(fc_snaive_walmart_monthly_HOUSEHOLD_2, test_monthly_HOUSEHOLD_2)

fc_snaive_walmart_monthly_FOODS <- snaive(train_monthly_FOODS, h=month_h)$mean
accuracy(fc_snaive_walmart_monthly_FOODS, test_monthly_FOODS)

fc_snaive_walmart_monthly_HOBBIES <- snaive(train_monthly_HOBBIES, h=month_h)$mean
accuracy(fc_snaive_walmart_monthly_HOBBIES, test_monthly_HOBBIES)

fc_snaive_walmart_monthly_HOUSEHOLD <- snaive(train_monthly_HOUSEHOLD, h=month_h)$mean
accuracy(fc_snaive_walmart_monthly_HOUSEHOLD, test_monthly_HOUSEHOLD)

fc_snaive_walmart_monthly_CA <- snaive(train_monthly_CA, h=month_h)$mean
accuracy(fc_snaive_walmart_monthly_CA, test_monthly_CA)

fc_snaive_walmart_monthly_TX <- snaive(train_monthly_TX, h=month_h)$mean
accuracy(fc_snaive_walmart_monthly_TX, test_monthly_TX)

fc_snaive_walmart_monthly_WI <- snaive(train_monthly_WI, h=month_h)$mean
accuracy(fc_snaive_walmart_monthly_WI, test_monthly_WI)



# ETS Model for Weekly series ---------------------------------------------

ets_walmart_weekly_FOODS_1 <- es(train_weekly_FOODS_1, model = "ZZZ")
fc_ets_walmart_weekly_FOODS_1 <- forecast(ets_walmart_weekly_FOODS_1, h=week_h)$mean
accuracy(fc_ets_walmart_weekly_FOODS_1, test_weekly_FOODS_1)
ets_walmart_weekly_FOODS_1

ets_walmart_weekly_FOODS_2 <- es(train_weekly_FOODS_2, model = "ZZZ")
fc_ets_walmart_weekly_FOODS_2 <- forecast(ets_walmart_weekly_FOODS_2, h=week_h)$mean
accuracy(fc_ets_walmart_weekly_FOODS_2, test_weekly_FOODS_2)
ets_walmart_weekly_FOODS_2

ets_walmart_weekly_FOODS_3 <- es(train_weekly_FOODS_3, model = "ZZZ")
fc_ets_walmart_weekly_FOODS_3 <- forecast(ets_walmart_weekly_FOODS_3, h=week_h)$mean
accuracy(fc_ets_walmart_weekly_FOODS_3, test_weekly_FOODS_3)
ets_walmart_weekly_FOODS_3

ets_walmart_weekly_HOBBIES_1 <- es(train_weekly_HOBBIES_1, model = "ZZZ")
fc_ets_walmart_weekly_HOBBIES_1 <- forecast(ets_walmart_weekly_HOBBIES_1, h=week_h)$mean
accuracy(fc_ets_walmart_weekly_HOBBIES_1, test_weekly_HOBBIES_1)
ets_walmart_weekly_HOBBIES_1

ets_walmart_weekly_HOBBIES_2 <- es(train_weekly_HOBBIES_2, model = "ZZZ")
fc_ets_walmart_weekly_HOBBIES_2 <- forecast(ets_walmart_weekly_HOBBIES_2, h=week_h)$mean
accuracy(fc_ets_walmart_weekly_HOBBIES_2, test_weekly_HOBBIES_2)
ets_walmart_weekly_HOBBIES_2

ets_walmart_weekly_HOUSEHOLD_1 <- es(train_weekly_HOUSEHOLD_1, model = "ZZZ")
fc_ets_walmart_weekly_HOUSEHOLD_1 <- forecast(ets_walmart_weekly_HOUSEHOLD_1, h=week_h)$mean
accuracy(fc_ets_walmart_weekly_HOUSEHOLD_1, test_weekly_HOUSEHOLD_1)
ets_walmart_weekly_HOUSEHOLD_1

ets_walmart_weekly_HOUSEHOLD_2 <- es(train_weekly_HOUSEHOLD_2, model = "ZZZ")
fc_ets_walmart_weekly_HOUSEHOLD_2<- forecast(ets_walmart_weekly_HOUSEHOLD_2, h=week_h)$mean
accuracy(fc_ets_walmart_weekly_HOUSEHOLD_2, test_weekly_HOUSEHOLD_2)
ets_walmart_weekly_HOUSEHOLD_2

ets_walmart_weekly_FOODS <- es(train_weekly_FOODS, model = "ZZZ")
fc_ets_walmart_weekly_FOODS<- forecast(ets_walmart_weekly_FOODS, h=week_h)$mean
accuracy(fc_ets_walmart_weekly_FOODS, test_weekly_FOODS)

ets_walmart_weekly_HOBBIES <- es(train_weekly_HOBBIES, model = "ZZZ")
fc_ets_walmart_weekly_HOBBIES<- forecast(ets_walmart_weekly_HOBBIES, h=week_h)$mean
accuracy(fc_ets_walmart_weekly_HOBBIES, test_weekly_HOBBIES)

ets_walmart_weekly_HOUSEHOLD <- es(train_weekly_HOUSEHOLD, model = "ZZZ")
fc_ets_walmart_weekly_HOUSEHOLD<- forecast(ets_walmart_weekly_HOUSEHOLD, h=week_h)$mean
accuracy(fc_ets_walmart_weekly_HOUSEHOLD, test_weekly_HOUSEHOLD)

ets_walmart_weekly_CA <- es(train_weekly_CA, model = "ZZZ")
fc_ets_walmart_weekly_CA<- forecast(ets_walmart_weekly_CA, h=week_h)$mean
accuracy(fc_ets_walmart_weekly_CA, test_weekly_CA)

ets_walmart_weekly_TX <- es(train_weekly_TX, model = "ZZZ")
fc_ets_walmart_weekly_TX<- forecast(ets_walmart_weekly_TX, h=week_h)$mean
accuracy(fc_ets_walmart_weekly_TX, test_weekly_TX)

ets_walmart_weekly_WI <- es(train_weekly_WI, model = "ZZZ")
fc_ets_walmart_weekly_WI<- forecast(ets_walmart_weekly_WI, h=week_h)$mean
accuracy(fc_ets_walmart_weekly_WI, test_weekly_WI)

# ETS Model for Monthly series ---------------------------------------------

ets_walmart_monthly_FOODS_1 <- es(train_monthly_FOODS_1, model = "ZZZ")
fc_ets_walmart_monthly_FOODS_1 <- forecast(ets_walmart_monthly_FOODS_1, h=month_h)$mean
accuracy(fc_ets_walmart_monthly_FOODS_1, test_monthly_FOODS_1)
ets_walmart_monthly_FOODS_1

ets_walmart_monthly_FOODS_2 <- es(train_monthly_FOODS_2, model = "ZZZ")
fc_ets_walmart_monthly_FOODS_2 <- forecast(ets_walmart_monthly_FOODS_2, h=month_h)$mean
accuracy(fc_ets_walmart_monthly_FOODS_2, test_monthly_FOODS_2)
ets_walmart_monthly_FOODS_2

ets_walmart_monthly_FOODS_3 <- es(train_monthly_FOODS_3, model = "ZZZ")
fc_ets_walmart_monthly_FOODS_3 <- forecast(ets_walmart_monthly_FOODS_3, h=month_h)$mean
accuracy(fc_ets_walmart_monthly_FOODS_3, test_monthly_FOODS_3)
ets_walmart_monthly_FOODS_3

ets_walmart_monthly_HOBBIES_1 <- es(train_monthly_HOBBIES_1, model = "ZZZ")
fc_ets_walmart_monthly_HOBBIES_1 <- forecast(ets_walmart_monthly_HOBBIES_1, h=month_h)$mean
accuracy(fc_ets_walmart_monthly_HOBBIES_1, test_monthly_HOBBIES_1)
ets_walmart_monthly_HOBBIES_1

ets_walmart_monthly_HOBBIES_2 <- es(train_monthly_HOBBIES_2, model = "ZZZ")
fc_ets_walmart_monthly_HOBBIES_2 <- forecast(ets_walmart_monthly_HOBBIES_2, h=month_h)$mean
accuracy(fc_ets_walmart_monthly_HOBBIES_2, test_monthly_HOBBIES_2)
ets_walmart_monthly_HOBBIES_2

ets_walmart_monthly_HOUSEHOLD_1 <- es(train_monthly_HOUSEHOLD_1, model = "ZZZ")
fc_ets_walmart_monthly_HOUSEHOLD_1 <- forecast(ets_walmart_monthly_HOUSEHOLD_1, h=month_h)$mean
accuracy(fc_ets_walmart_monthly_HOUSEHOLD_1, test_monthly_HOUSEHOLD_1)
ets_walmart_monthly_HOUSEHOLD_1

ets_walmart_monthly_HOUSEHOLD_2 <- es(train_monthly_HOUSEHOLD_2, model = "ZZZ")
fc_ets_walmart_monthly_HOUSEHOLD_2<- forecast(ets_walmart_monthly_HOUSEHOLD_2, h=month_h)$mean
accuracy(fc_ets_walmart_monthly_HOUSEHOLD_2, test_monthly_HOUSEHOLD_2)
ets_walmart_monthly_HOUSEHOLD_2

ets_walmart_monthly_FOODS <- es(train_monthly_FOODS, model = "ZZZ")
fc_ets_walmart_monthly_FOODS<- forecast(ets_walmart_monthly_FOODS, h=month_h)$mean
accuracy(fc_ets_walmart_monthly_FOODS, test_monthly_FOODS)

ets_walmart_monthly_HOBBIES <- es(train_monthly_HOBBIES, model = "ZZZ")
fc_ets_walmart_monthly_HOBBIES<- forecast(ets_walmart_monthly_HOBBIES, h=month_h)$mean
accuracy(fc_ets_walmart_monthly_HOBBIES, test_monthly_HOBBIES)

ets_walmart_monthly_HOUSEHOLD <- es(train_monthly_HOUSEHOLD, model = "ZZZ")
fc_ets_walmart_monthly_HOUSEHOLD<- forecast(ets_walmart_monthly_HOUSEHOLD, h=month_h)$mean
accuracy(fc_ets_walmart_monthly_HOUSEHOLD, test_monthly_HOUSEHOLD)

ets_walmart_monthly_CA <- es(train_monthly_CA, model = "ZZZ")
fc_ets_walmart_monthly_CA<- forecast(ets_walmart_monthly_CA, h=month_h)$mean
accuracy(fc_ets_walmart_monthly_CA, test_monthly_CA)

ets_walmart_monthly_TX <- es(train_monthly_TX, model = "ZZZ")
fc_ets_walmart_monthly_TX<- forecast(ets_walmart_monthly_TX, h=month_h)$mean
accuracy(fc_ets_walmart_monthly_TX, test_monthly_TX)

ets_walmart_monthly_WI <- es(train_monthly_WI, model = "ZZZ")
fc_ets_walmart_monthly_WI<- forecast(ets_walmart_monthly_WI, h=month_h)$mean
accuracy(fc_ets_walmart_monthly_WI, test_monthly_WI)


# Seasonal ARIMA Model for Weekly series  --------------------------------

arima_walmart_weekly_FOODS_1 <- auto.ssarima(train_weekly_FOODS_1)
fc_arima_walmart_weekly_FOODS_1 <- forecast(arima_walmart_weekly_FOODS_1, h=week_h)$mean
accuracy(fc_arima_walmart_weekly_FOODS_1, test_weekly_FOODS_1)
arima_walmart_weekly_FOODS_1

arima_walmart_weekly_FOODS_2 <- auto.ssarima(train_weekly_FOODS_2, model = "ZZZ")
fc_arima_walmart_weekly_FOODS_2 <- forecast(arima_walmart_weekly_FOODS_2, h=week_h)$mean
accuracy(fc_arima_walmart_weekly_FOODS_2, test_weekly_FOODS_2)
arima_walmart_weekly_FOODS_2

arima_walmart_weekly_FOODS_3 <- auto.ssarima(train_weekly_FOODS_3, model = "ZZZ")
fc_arima_walmart_weekly_FOODS_3 <- forecast(arima_walmart_weekly_FOODS_3, h=week_h)$mean
accuracy(fc_arima_walmart_weekly_FOODS_3, test_weekly_FOODS_3)
arima_walmart_weekly_FOODS_3

arima_walmart_weekly_HOBBIES_1 <- auto.ssarima(train_weekly_HOBBIES_1, model = "ZZZ")
fc_arima_walmart_weekly_HOBBIES_1 <- forecast(arima_walmart_weekly_HOBBIES_1, h=week_h)$mean
accuracy(fc_arima_walmart_weekly_HOBBIES_1, test_weekly_HOBBIES_1)
arima_walmart_weekly_HOBBIES_1

arima_walmart_weekly_HOBBIES_2 <- auto.ssarima(train_weekly_HOBBIES_2, model = "ZZZ")
fc_arima_walmart_weekly_HOBBIES_2 <- forecast(arima_walmart_weekly_HOBBIES_2, h=week_h)$mean
accuracy(fc_arima_walmart_weekly_HOBBIES_2, test_weekly_HOBBIES_2)
arima_walmart_weekly_HOBBIES_2

arima_walmart_weekly_HOUSEHOLD_1 <- auto.ssarima(train_weekly_HOUSEHOLD_1, model = "ZZZ")
fc_arima_walmart_weekly_HOUSEHOLD_1 <- forecast(arima_walmart_weekly_HOUSEHOLD_1, h=week_h)$mean
accuracy(fc_arima_walmart_weekly_HOUSEHOLD_1, test_weekly_HOUSEHOLD_1)
arima_walmart_weekly_HOUSEHOLD_1

arima_walmart_weekly_HOUSEHOLD_2 <- auto.ssarima(train_weekly_HOUSEHOLD_2, model = "ZZZ")
fc_arima_walmart_weekly_HOUSEHOLD_2<- forecast(arima_walmart_weekly_HOUSEHOLD_2, h=week_h)$mean
accuracy(fc_arima_walmart_weekly_HOUSEHOLD_2, test_weekly_HOUSEHOLD_2)
arima_walmart_weekly_HOUSEHOLD_2

arima_walmart_weekly_FOODS <- auto.ssarima(train_weekly_FOODS, model = "ZZZ")
fc_arima_walmart_weekly_FOODS<- forecast(arima_walmart_weekly_FOODS, h=week_h)$mean
accuracy(fc_arima_walmart_weekly_FOODS, test_weekly_FOODS)

arima_walmart_weekly_HOBBIES <- auto.ssarima(train_weekly_HOBBIES, model = "ZZZ")
fc_arima_walmart_weekly_HOBBIES<- forecast(arima_walmart_weekly_HOBBIES, h=week_h)$mean
accuracy(fc_arima_walmart_weekly_HOBBIES, test_weekly_HOBBIES)

arima_walmart_weekly_HOUSEHOLD <- auto.ssarima(train_weekly_HOUSEHOLD, model = "ZZZ")
fc_arima_walmart_weekly_HOUSEHOLD<- forecast(arima_walmart_weekly_HOUSEHOLD, h=week_h)$mean
accuracy(fc_arima_walmart_weekly_HOUSEHOLD, test_weekly_HOUSEHOLD)

arima_walmart_weekly_CA <- auto.ssarima(train_weekly_CA, model = "ZZZ")
fc_arima_walmart_weekly_CA<- forecast(arima_walmart_weekly_CA, h=week_h)$mean
accuracy(fc_arima_walmart_weekly_CA, test_weekly_CA)

arima_walmart_weekly_TX <- auto.ssarima(train_weekly_TX, model = "ZZZ")
fc_arima_walmart_weekly_TX<- forecast(arima_walmart_weekly_TX, h=week_h)$mean
accuracy(fc_arima_walmart_weekly_TX, test_weekly_TX)

arima_walmart_weekly_WI <- auto.ssarima(train_weekly_WI, model = "ZZZ")
fc_arima_walmart_weekly_WI<- forecast(arima_walmart_weekly_WI, h=week_h)$mean
accuracy(fc_arima_walmart_weekly_WI, test_weekly_WI)

# Seasonal ARIMA Model for Monthly series  --------------------------------

arima_walmart_monthly_FOODS_1 <- auto.ssarima(train_monthly_FOODS_1)
fc_arima_walmart_monthly_FOODS_1 <- forecast(arima_walmart_monthly_FOODS_1, h=month_h)$mean
accuracy(fc_arima_walmart_monthly_FOODS_1, test_monthly_FOODS_1)
arima_walmart_monthly_FOODS_1

arima_walmart_monthly_FOODS_2 <- auto.ssarima(train_monthly_FOODS_2, model = "ZZZ")
fc_arima_walmart_monthly_FOODS_2 <- forecast(arima_walmart_monthly_FOODS_2, h=month_h)$mean
accuracy(fc_arima_walmart_monthly_FOODS_2, test_monthly_FOODS_2)
arima_walmart_monthly_FOODS_2

arima_walmart_monthly_FOODS_3 <- auto.ssarima(train_monthly_FOODS_3, model = "ZZZ")
fc_arima_walmart_monthly_FOODS_3 <- forecast(arima_walmart_monthly_FOODS_3, h=month_h)$mean
accuracy(fc_arima_walmart_monthly_FOODS_3, test_monthly_FOODS_3)
arima_walmart_monthly_FOODS_3

arima_walmart_monthly_HOBBIES_1 <- auto.ssarima(train_monthly_HOBBIES_1, model = "ZZZ")
fc_arima_walmart_monthly_HOBBIES_1 <- forecast(arima_walmart_monthly_HOBBIES_1, h=month_h)$mean
accuracy(fc_arima_walmart_monthly_HOBBIES_1, test_monthly_HOBBIES_1)
arima_walmart_monthly_HOBBIES_1

arima_walmart_monthly_HOBBIES_2 <- auto.ssarima(train_monthly_HOBBIES_2, model = "ZZZ")
fc_arima_walmart_monthly_HOBBIES_2 <- forecast(arima_walmart_monthly_HOBBIES_2, h=month_h)$mean
accuracy(fc_arima_walmart_monthly_HOBBIES_2, test_monthly_HOBBIES_2)
arima_walmart_monthly_HOBBIES_2

arima_walmart_monthly_HOUSEHOLD_1 <- auto.ssarima(train_monthly_HOUSEHOLD_1, model = "ZZZ")
fc_arima_walmart_monthly_HOUSEHOLD_1 <- forecast(arima_walmart_monthly_HOUSEHOLD_1, h=month_h)$mean
accuracy(fc_arima_walmart_monthly_HOUSEHOLD_1, test_monthly_HOUSEHOLD_1)
arima_walmart_monthly_HOUSEHOLD_1

arima_walmart_monthly_HOUSEHOLD_2 <- auto.ssarima(train_monthly_HOUSEHOLD_2, model = "ZZZ")
fc_arima_walmart_monthly_HOUSEHOLD_2<- forecast(arima_walmart_monthly_HOUSEHOLD_2, h=month_h)$mean
accuracy(fc_arima_walmart_monthly_HOUSEHOLD_2, test_monthly_HOUSEHOLD_2)
arima_walmart_monthly_HOUSEHOLD_2

arima_walmart_monthly_FOODS <- auto.ssarima(train_monthly_FOODS, model = "ZZZ")
fc_arima_walmart_monthly_FOODS<- forecast(arima_walmart_monthly_FOODS, h=month_h)$mean
accuracy(fc_arima_walmart_monthly_FOODS, test_monthly_FOODS)

arima_walmart_monthly_HOBBIES <- auto.ssarima(train_monthly_HOBBIES, model = "ZZZ")
fc_arima_walmart_monthly_HOBBIES<- forecast(arima_walmart_monthly_HOBBIES, h=month_h)$mean
accuracy(fc_arima_walmart_monthly_HOBBIES, test_monthly_HOBBIES)

arima_walmart_monthly_HOUSEHOLD <- auto.ssarima(train_monthly_HOUSEHOLD, model = "ZZZ")
fc_arima_walmart_monthly_HOUSEHOLD<- forecast(arima_walmart_monthly_HOUSEHOLD, h=month_h)$mean
accuracy(fc_arima_walmart_monthly_HOUSEHOLD, test_monthly_HOUSEHOLD)

arima_walmart_monthly_CA <- auto.ssarima(train_monthly_CA, model = "ZZZ")
fc_arima_walmart_monthly_CA<- forecast(arima_walmart_monthly_CA, h=month_h)$mean
accuracy(fc_arima_walmart_monthly_CA, test_monthly_CA)

arima_walmart_monthly_TX <- auto.ssarima(train_monthly_TX, model = "ZZZ")
fc_arima_walmart_monthly_TX<- forecast(arima_walmart_monthly_TX, h=month_h)$mean
accuracy(fc_arima_walmart_monthly_TX, test_monthly_TX)

arima_walmart_monthly_WI <- auto.ssarima(train_monthly_WI, model = "ZZZ")
fc_arima_walmart_monthly_WI<- forecast(arima_walmart_monthly_WI, h=month_h)$mean
accuracy(fc_arima_walmart_monthly_WI, test_monthly_WI)


Summary_stats <- matrix(c(accuracy(fc_arima_walmart_monthly_WI, test_monthly_WI)), ncol=5, byrow=TRUE)
colnames(Summary_stats) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(Summary_stats) <- c('ARIMA Model for Monthly freq. of WI dataset')
Summary_stats <- as.table(Summary_stats)
Summary_stats <- rbind(Summary_stats,c(accuracy(fc_arima_walmart_monthly_TX, test_monthly_TX)))
Summary_stats
knitr::kable(Summary_stats)


# Summary tables ----------------------------------------------------------

## Summary table for FOODS 1 ----------------------------------------------------------
summary_weekly_FOODS_1 <- matrix(c(accuracy(fc_snaive_walmart_weekly_FOODS_1, test_weekly_FOODS_1)), ncol=5, byrow=TRUE)
summary_weekly_FOODS_1 <- as.table(summary_weekly_FOODS_1)
summary_weekly_FOODS_1 <- rbind(summary_weekly_FOODS_1,c(accuracy(fc_ets_walmart_weekly_FOODS_1, test_weekly_FOODS_1)))
summary_weekly_FOODS_1 <- rbind(summary_weekly_FOODS_1,c(accuracy(fc_arima_walmart_weekly_FOODS_1, test_weekly_FOODS_1)))
colnames(summary_weekly_FOODS_1) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_weekly_FOODS_1) <- c('Seasonal Naive Model for Weekly freq. of FOODS 1 dataset','ETS Model for Weekly freq. of FOODS 1 dataset','ARIMA Model for Weekly freq. of FOODS 1 dataset')
summary_weekly_FOODS_1
knitr::kable(summary_weekly_FOODS_1)

summary_monthly_FOODS_1 <- matrix(c(accuracy(fc_snaive_walmart_monthly_FOODS_1, test_monthly_FOODS_1)), ncol=5, byrow=TRUE)
summary_monthly_FOODS_1 <- as.table(summary_monthly_FOODS_1)
summary_monthly_FOODS_1 <- rbind(summary_monthly_FOODS_1,c(accuracy(fc_ets_walmart_monthly_FOODS_1, test_monthly_FOODS_1)))
summary_monthly_FOODS_1 <- rbind(summary_monthly_FOODS_1,c(accuracy(fc_arima_walmart_monthly_FOODS_1, test_monthly_FOODS_1)))
colnames(summary_monthly_FOODS_1) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_monthly_FOODS_1) <- c('Seasonal Naive Model for Monthly freq. of FOODS 1 dataset','ETS Model for Monthly freq. of FOODS 1 dataset','ARIMA Model for Monthly freq. of FOODS 1 dataset')
summary_monthly_FOODS_1
knitr::kable(summary_monthly_FOODS_1)

## Summary table for FOODS 2 ----------------------------------------------------------

summary_weekly_FOODS_2 <- matrix(c(accuracy(fc_snaive_walmart_weekly_FOODS_2, test_weekly_FOODS_2)), ncol=5, byrow=TRUE)
summary_weekly_FOODS_2 <- as.table(summary_weekly_FOODS_2)
summary_weekly_FOODS_2 <- rbind(summary_weekly_FOODS_2,c(accuracy(fc_ets_walmart_weekly_FOODS_2, test_weekly_FOODS_2)))
summary_weekly_FOODS_2 <- rbind(summary_weekly_FOODS_2,c(accuracy(fc_arima_walmart_weekly_FOODS_2, test_weekly_FOODS_2)))
colnames(summary_weekly_FOODS_2) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_weekly_FOODS_2) <- c('Seasonal Naive Model for Weekly freq. of FOODS 2 dataset','ETS Model for Weekly freq. of FOODS 2 dataset','ARIMA Model for Weekly freq. of FOODS 2 dataset')
summary_weekly_FOODS_2
knitr::kable(summary_weekly_FOODS_2)

summary_monthly_FOODS_2 <- matrix(c(accuracy(fc_snaive_walmart_monthly_FOODS_2, test_monthly_FOODS_2)), ncol=5, byrow=TRUE)
summary_monthly_FOODS_2 <- as.table(summary_monthly_FOODS_2)
summary_monthly_FOODS_2 <- rbind(summary_monthly_FOODS_2,c(accuracy(fc_ets_walmart_monthly_FOODS_2, test_monthly_FOODS_2)))
summary_monthly_FOODS_2 <- rbind(summary_monthly_FOODS_2,c(accuracy(fc_arima_walmart_monthly_FOODS_2, test_monthly_FOODS_2)))
colnames(summary_monthly_FOODS_2) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_monthly_FOODS_2) <- c('Seasonal Naive Model for Monthly freq. of FOODS 2 dataset','ETS Model for Monthly freq. of FOODS 2 dataset','ARIMA Model for Monthly freq. of FOODS 2 dataset')
summary_monthly_FOODS_2
knitr::kable(summary_monthly_FOODS_2)


## Summary table for FOODS 3 ----------------------------------------------------------

summary_weekly_FOODS_3 <- matrix(c(accuracy(fc_snaive_walmart_weekly_FOODS_3, test_weekly_FOODS_3)), ncol=5, byrow=TRUE)
summary_weekly_FOODS_3 <- as.table(summary_weekly_FOODS_3)
summary_weekly_FOODS_3 <- rbind(summary_weekly_FOODS_3,c(accuracy(fc_ets_walmart_weekly_FOODS_3, test_weekly_FOODS_3)))
summary_weekly_FOODS_3 <- rbind(summary_weekly_FOODS_3,c(accuracy(fc_arima_walmart_weekly_FOODS_3, test_weekly_FOODS_3)))
colnames(summary_weekly_FOODS_3) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_weekly_FOODS_3) <- c('Seasonal Naive Model for Weekly freq. of FOODS 3 dataset','ETS Model for Weekly freq. of FOODS 3 dataset','ARIMA Model for Weekly freq. of FOODS 3 dataset')
summary_weekly_FOODS_3
knitr::kable(summary_weekly_FOODS_3)

summary_monthly_FOODS_3 <- matrix(c(accuracy(fc_snaive_walmart_monthly_FOODS_3, test_monthly_FOODS_3)), ncol=5, byrow=TRUE)
summary_monthly_FOODS_3 <- as.table(summary_monthly_FOODS_3)
summary_monthly_FOODS_3 <- rbind(summary_monthly_FOODS_3,c(accuracy(fc_ets_walmart_monthly_FOODS_3, test_monthly_FOODS_3)))
summary_monthly_FOODS_3 <- rbind(summary_monthly_FOODS_3,c(accuracy(fc_arima_walmart_monthly_FOODS_3, test_monthly_FOODS_3)))
colnames(summary_monthly_FOODS_3) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_monthly_FOODS_3) <- c('Seasonal Naive Model for Monthly freq. of FOODS 3 dataset','ETS Model for Monthly freq. of FOODS 3 dataset','ARIMA Model for Monthly freq. of FOODS 3 dataset')
summary_monthly_FOODS_3
knitr::kable(summary_monthly_FOODS_3)


## Summary table for HOBBIES 1 ----------------------------------------------------------

summary_weekly_HOBBIES_1 <- matrix(c(accuracy(fc_snaive_walmart_weekly_HOBBIES_1, test_weekly_HOBBIES_1)), ncol=5, byrow=TRUE)
summary_weekly_HOBBIES_1 <- as.table(summary_weekly_HOBBIES_1)
summary_weekly_HOBBIES_1 <- rbind(summary_weekly_HOBBIES_1,c(accuracy(fc_ets_walmart_weekly_HOBBIES_1, test_weekly_HOBBIES_1)))
summary_weekly_HOBBIES_1 <- rbind(summary_weekly_HOBBIES_1,c(accuracy(fc_arima_walmart_weekly_HOBBIES_1, test_weekly_HOBBIES_1)))
colnames(summary_weekly_HOBBIES_1) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_weekly_HOBBIES_1) <- c('Seasonal Naive Model for Weekly freq. of HOBBIES 1 dataset','ETS Model for Weekly freq. of HOBBIES 1 dataset','ARIMA Model for Weekly freq. of HOBBIES 1 dataset')
summary_weekly_HOBBIES_1
knitr::kable(summary_weekly_HOBBIES_1)

summary_monthly_HOBBIES_1 <- matrix(c(accuracy(fc_snaive_walmart_monthly_HOBBIES_1, test_monthly_HOBBIES_1)), ncol=5, byrow=TRUE)
summary_monthly_HOBBIES_1 <- as.table(summary_monthly_HOBBIES_1)
summary_monthly_HOBBIES_1 <- rbind(summary_monthly_HOBBIES_1,c(accuracy(fc_ets_walmart_monthly_HOBBIES_1, test_monthly_HOBBIES_1)))
summary_monthly_HOBBIES_1 <- rbind(summary_monthly_HOBBIES_1,c(accuracy(fc_arima_walmart_monthly_HOBBIES_1, test_monthly_HOBBIES_1)))
colnames(summary_monthly_HOBBIES_1) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_monthly_HOBBIES_1) <- c('Seasonal Naive Model for Monthly freq. of HOBBIES 1 dataset','ETS Model for Monthly freq. of HOBBIES 1 dataset','ARIMA Model for Monthly freq. of HOBBIES 1 dataset')
summary_monthly_HOBBIES_1
knitr::kable(summary_monthly_HOBBIES_1)


## Summary table for HOBBIES 2 ----------------------------------------------------------


summary_weekly_HOBBIES_2 <- matrix(c(accuracy(fc_snaive_walmart_weekly_HOBBIES_2, test_weekly_HOBBIES_2)), ncol=5, byrow=TRUE)
summary_weekly_HOBBIES_2 <- as.table(summary_weekly_HOBBIES_2)
summary_weekly_HOBBIES_2 <- rbind(summary_weekly_HOBBIES_2,c(accuracy(fc_ets_walmart_weekly_HOBBIES_2, test_weekly_HOBBIES_2)))
summary_weekly_HOBBIES_2 <- rbind(summary_weekly_HOBBIES_2,c(accuracy(fc_arima_walmart_weekly_HOBBIES_2, test_weekly_HOBBIES_2)))
colnames(summary_weekly_HOBBIES_2) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_weekly_HOBBIES_2) <- c('Seasonal Naive Model for Weekly freq. of HOBBIES 2 dataset','ETS Model for Weekly freq. of HOBBIES 2 dataset','ARIMA Model for Weekly freq. of HOBBIES 2 dataset')
summary_weekly_HOBBIES_2
knitr::kable(summary_weekly_HOBBIES_2)

summary_monthly_HOBBIES_2 <- matrix(c(accuracy(fc_snaive_walmart_monthly_HOBBIES_2, test_monthly_HOBBIES_2)), ncol=5, byrow=TRUE)
summary_monthly_HOBBIES_2 <- as.table(summary_monthly_HOBBIES_2)
summary_monthly_HOBBIES_2 <- rbind(summary_monthly_HOBBIES_2,c(accuracy(fc_ets_walmart_monthly_HOBBIES_2, test_monthly_HOBBIES_2)))
summary_monthly_HOBBIES_2 <- rbind(summary_monthly_HOBBIES_2,c(accuracy(fc_arima_walmart_monthly_HOBBIES_2, test_monthly_HOBBIES_2)))
colnames(summary_monthly_HOBBIES_2) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_monthly_HOBBIES_2) <- c('Seasonal Naive Model for Monthly freq. of HOBBIES 2 dataset','ETS Model for Monthly freq. of HOBBIES 2 dataset','ARIMA Model for Monthly freq. of HOBBIES 2 dataset')
summary_monthly_HOBBIES_2
knitr::kable(summary_monthly_HOBBIES_2)

## Summary table for HOUSEHOLD 1 ----------------------------------------------------------


summary_weekly_HOUSEHOLD_1 <- matrix(c(accuracy(fc_snaive_walmart_weekly_HOUSEHOLD_1, test_weekly_HOUSEHOLD_1)), ncol=5, byrow=TRUE)
summary_weekly_HOUSEHOLD_1 <- as.table(summary_weekly_HOUSEHOLD_1)
summary_weekly_HOUSEHOLD_1 <- rbind(summary_weekly_HOUSEHOLD_1,c(accuracy(fc_ets_walmart_weekly_HOUSEHOLD_1, test_weekly_HOUSEHOLD_1)))
summary_weekly_HOUSEHOLD_1 <- rbind(summary_weekly_HOUSEHOLD_1,c(accuracy(fc_arima_walmart_weekly_HOUSEHOLD_1, test_weekly_HOUSEHOLD_1)))
colnames(summary_weekly_HOUSEHOLD_1) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_weekly_HOUSEHOLD_1) <- c('Seasonal Naive Model for Weekly freq. of HOUSEHOLD 1 dataset','ETS Model for Weekly freq. of HOUSEHOLD 1 dataset','ARIMA Model for Weekly freq. of HOUSEHOLD 1 dataset')
summary_weekly_HOUSEHOLD_1
knitr::kable(summary_weekly_HOUSEHOLD_1)

summary_monthly_HOUSEHOLD_1 <- matrix(c(accuracy(fc_snaive_walmart_monthly_HOUSEHOLD_1, test_monthly_HOUSEHOLD_1)), ncol=5, byrow=TRUE)
summary_monthly_HOUSEHOLD_1 <- as.table(summary_monthly_HOUSEHOLD_1)
summary_monthly_HOUSEHOLD_1 <- rbind(summary_monthly_HOUSEHOLD_1,c(accuracy(fc_ets_walmart_monthly_HOUSEHOLD_1, test_monthly_HOUSEHOLD_1)))
summary_monthly_HOUSEHOLD_1 <- rbind(summary_monthly_HOUSEHOLD_1,c(accuracy(fc_arima_walmart_monthly_HOUSEHOLD_1, test_monthly_HOUSEHOLD_1)))
colnames(summary_monthly_HOUSEHOLD_1) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_monthly_HOUSEHOLD_1) <- c('Seasonal Naive Model for Monthly freq. of HOUSEHOLD 1 dataset','ETS Model for Monthly freq. of HOUSEHOLD 1 dataset','ARIMA Model for Monthly freq. of HOUSEHOLD 1 dataset')
summary_monthly_HOUSEHOLD_1
knitr::kable(summary_monthly_HOUSEHOLD_1)

## Summary table for HOUSEHOLD 2 ----------------------------------------------------------

summary_weekly_HOUSEHOLD_2 <- matrix(c(accuracy(fc_snaive_walmart_weekly_HOUSEHOLD_2, test_weekly_HOUSEHOLD_2)), ncol=5, byrow=TRUE)
summary_weekly_HOUSEHOLD_2 <- as.table(summary_weekly_HOUSEHOLD_2)
summary_weekly_HOUSEHOLD_2 <- rbind(summary_weekly_HOUSEHOLD_2,c(accuracy(fc_ets_walmart_weekly_HOUSEHOLD_2, test_weekly_HOUSEHOLD_2)))
summary_weekly_HOUSEHOLD_2 <- rbind(summary_weekly_HOUSEHOLD_2,c(accuracy(fc_arima_walmart_weekly_HOUSEHOLD_2, test_weekly_HOUSEHOLD_2)))
colnames(summary_weekly_HOUSEHOLD_2) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_weekly_HOUSEHOLD_2) <- c('Seasonal Naive Model for Weekly freq. of HOUSEHOLD 2 dataset','ETS Model for Weekly freq. of HOUSEHOLD 2 dataset','ARIMA Model for Weekly freq. of HOUSEHOLD 2 dataset')
summary_weekly_HOUSEHOLD_2
knitr::kable(summary_weekly_HOUSEHOLD_2)

summary_monthly_HOUSEHOLD_2 <- matrix(c(accuracy(fc_snaive_walmart_monthly_HOUSEHOLD_2, test_monthly_HOUSEHOLD_2)), ncol=5, byrow=TRUE)
summary_monthly_HOUSEHOLD_2 <- as.table(summary_monthly_HOUSEHOLD_2)
summary_monthly_HOUSEHOLD_2 <- rbind(summary_monthly_HOUSEHOLD_2,c(accuracy(fc_ets_walmart_monthly_HOUSEHOLD_2, test_monthly_HOUSEHOLD_2)))
summary_monthly_HOUSEHOLD_2 <- rbind(summary_monthly_HOUSEHOLD_2,c(accuracy(fc_arima_walmart_monthly_HOUSEHOLD_2, test_monthly_HOUSEHOLD_2)))
colnames(summary_monthly_HOUSEHOLD_2) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_monthly_HOUSEHOLD_2) <- c('Seasonal Naive Model for Monthly freq. of HOUSEHOLD 2 dataset','ETS Model for Monthly freq. of HOUSEHOLD 2 dataset','ARIMA Model for Monthly freq. of HOUSEHOLD 2 dataset')
summary_monthly_HOUSEHOLD_2
knitr::kable(summary_monthly_HOUSEHOLD_2)


## Summary table for FOODS  ----------------------------------------------------------

summary_weekly_FOODS <- matrix(c(accuracy(fc_snaive_walmart_weekly_FOODS, test_weekly_FOODS)), ncol=5, byrow=TRUE)
summary_weekly_FOODS <- as.table(summary_weekly_FOODS)
summary_weekly_FOODS <- rbind(summary_weekly_FOODS,c(accuracy(fc_ets_walmart_weekly_FOODS, test_weekly_FOODS)))
summary_weekly_FOODS <- rbind(summary_weekly_FOODS,c(accuracy(fc_arima_walmart_weekly_FOODS, test_weekly_FOODS)))
colnames(summary_weekly_FOODS) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_weekly_FOODS) <- c('Seasonal Naive Model for Weekly freq. of FOODS dataset','ETS Model for Weekly freq. of FOODS dataset','ARIMA Model for Weekly freq. of FOODS dataset')
summary_weekly_FOODS
knitr::kable(summary_weekly_FOODS)

summary_monthly_FOODS <- matrix(c(accuracy(fc_snaive_walmart_monthly_FOODS, test_monthly_FOODS)), ncol=5, byrow=TRUE)
summary_monthly_FOODS <- as.table(summary_monthly_FOODS)
summary_monthly_FOODS <- rbind(summary_monthly_FOODS,c(accuracy(fc_ets_walmart_monthly_FOODS, test_monthly_FOODS)))
summary_monthly_FOODS <- rbind(summary_monthly_FOODS,c(accuracy(fc_arima_walmart_monthly_FOODS, test_monthly_FOODS)))
colnames(summary_monthly_FOODS) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_monthly_FOODS) <- c('Seasonal Naive Model for Monthly freq. of FOODS dataset','ETS Model for Monthly freq. of FOODS dataset','ARIMA Model for Monthly freq. of FOODS dataset')
summary_monthly_FOODS
knitr::kable(summary_monthly_FOODS)


## Summary table for HOBBIES  ----------------------------------------------------------

summary_weekly_HOBBIES <- matrix(c(accuracy(fc_snaive_walmart_weekly_HOBBIES, test_weekly_HOBBIES)), ncol=5, byrow=TRUE)
summary_weekly_HOBBIES <- as.table(summary_weekly_HOBBIES)
summary_weekly_HOBBIES <- rbind(summary_weekly_HOBBIES,c(accuracy(fc_ets_walmart_weekly_HOBBIES, test_weekly_HOBBIES)))
summary_weekly_HOBBIES <- rbind(summary_weekly_HOBBIES,c(accuracy(fc_arima_walmart_weekly_HOBBIES, test_weekly_HOBBIES)))
colnames(summary_weekly_HOBBIES) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_weekly_HOBBIES) <- c('Seasonal Naive Model for Weekly freq. of HOBBIES dataset','ETS Model for Weekly freq. of HOBBIES dataset','ARIMA Model for Weekly freq. of HOBBIES dataset')
summary_weekly_HOBBIES
knitr::kable(summary_weekly_HOBBIES)

summary_monthly_HOBBIES <- matrix(c(accuracy(fc_snaive_walmart_monthly_HOBBIES, test_monthly_HOBBIES)), ncol=5, byrow=TRUE)
summary_monthly_HOBBIES <- as.table(summary_monthly_HOBBIES)
summary_monthly_HOBBIES <- rbind(summary_monthly_HOBBIES,c(accuracy(fc_ets_walmart_monthly_HOBBIES, test_monthly_HOBBIES)))
summary_monthly_HOBBIES <- rbind(summary_monthly_HOBBIES,c(accuracy(fc_arima_walmart_monthly_HOBBIES, test_monthly_HOBBIES)))
colnames(summary_monthly_HOBBIES) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_monthly_HOBBIES) <- c('Seasonal Naive Model for Monthly freq. of HOBBIES dataset','ETS Model for Monthly freq. of HOBBIES dataset','ARIMA Model for Monthly freq. of HOBBIES dataset')
summary_monthly_HOBBIES
knitr::kable(summary_monthly_HOBBIES)


## Summary table for HOUSEHOLD  ----------------------------------------------------------

summary_weekly_HOUSEHOLD <- matrix(c(accuracy(fc_snaive_walmart_weekly_HOUSEHOLD, test_weekly_HOUSEHOLD)), ncol=5, byrow=TRUE)
summary_weekly_HOUSEHOLD <- as.table(summary_weekly_HOUSEHOLD)
summary_weekly_HOUSEHOLD <- rbind(summary_weekly_HOUSEHOLD,c(accuracy(fc_ets_walmart_weekly_HOUSEHOLD, test_weekly_HOUSEHOLD)))
summary_weekly_HOUSEHOLD <- rbind(summary_weekly_HOUSEHOLD,c(accuracy(fc_arima_walmart_weekly_HOUSEHOLD, test_weekly_HOUSEHOLD)))
colnames(summary_weekly_HOUSEHOLD) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_weekly_HOUSEHOLD) <- c('Seasonal Naive Model for Weekly freq. of HOUSEHOLD dataset','ETS Model for Weekly freq. of HOUSEHOLD dataset','ARIMA Model for Weekly freq. of HOUSEHOLD dataset')
summary_weekly_HOUSEHOLD
knitr::kable(summary_weekly_HOUSEHOLD)

summary_monthly_HOUSEHOLD <- matrix(c(accuracy(fc_snaive_walmart_monthly_HOUSEHOLD, test_monthly_HOUSEHOLD)), ncol=5, byrow=TRUE)
summary_monthly_HOUSEHOLD <- as.table(summary_monthly_HOUSEHOLD)
summary_monthly_HOUSEHOLD <- rbind(summary_monthly_HOUSEHOLD,c(accuracy(fc_ets_walmart_monthly_HOUSEHOLD, test_monthly_HOUSEHOLD)))
summary_monthly_HOUSEHOLD <- rbind(summary_monthly_HOUSEHOLD,c(accuracy(fc_arima_walmart_monthly_HOUSEHOLD, test_monthly_HOUSEHOLD)))
colnames(summary_monthly_HOUSEHOLD) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_monthly_HOUSEHOLD) <- c('Seasonal Naive Model for Monthly freq. of HOUSEHOLD dataset','ETS Model for Monthly freq. of HOUSEHOLD dataset','ARIMA Model for Monthly freq. of HOUSEHOLD dataset')
summary_monthly_HOUSEHOLD
knitr::kable(summary_monthly_HOUSEHOLD)


## Summary table for CA ----------------------------------------------------------

summary_weekly_CA <- matrix(c(accuracy(fc_snaive_walmart_weekly_CA, test_weekly_CA)), ncol=5, byrow=TRUE)
summary_weekly_CA <- as.table(summary_weekly_CA)
summary_weekly_CA <- rbind(summary_weekly_CA,c(accuracy(fc_ets_walmart_weekly_CA, test_weekly_CA)))
summary_weekly_CA <- rbind(summary_weekly_CA,c(accuracy(fc_arima_walmart_weekly_CA, test_weekly_CA)))
colnames(summary_weekly_CA) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_weekly_CA) <- c('Seasonal Naive Model for Weekly freq. of CA dataset','ETS Model for Weekly freq. of CA dataset','ARIMA Model for Weekly freq. of CA dataset')
summary_weekly_CA
knitr::kable(summary_weekly_CA)

summary_monthly_CA <- matrix(c(accuracy(fc_snaive_walmart_monthly_CA, test_monthly_CA)), ncol=5, byrow=TRUE)
summary_monthly_CA <- as.table(summary_monthly_CA)
summary_monthly_CA <- rbind(summary_monthly_CA,c(accuracy(fc_ets_walmart_monthly_CA, test_monthly_CA)))
summary_monthly_CA <- rbind(summary_monthly_CA,c(accuracy(fc_arima_walmart_monthly_CA, test_monthly_CA)))
colnames(summary_monthly_CA) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_monthly_CA) <- c('Seasonal Naive Model for Monthly freq. of CA dataset','ETS Model for Monthly freq. of CA dataset','ARIMA Model for Monthly freq. of CA dataset')
summary_monthly_CA
knitr::kable(summary_monthly_CA)


## Summary table for TX ----------------------------------------------------------

summary_weekly_TX <- matrix(c(accuracy(fc_snaive_walmart_weekly_TX, test_weekly_TX)), ncol=5, byrow=TRUE)
summary_weekly_TX <- as.table(summary_weekly_TX)
summary_weekly_TX <- rbind(summary_weekly_TX,c(accuracy(fc_ets_walmart_weekly_TX, test_weekly_TX)))
summary_weekly_TX <- rbind(summary_weekly_TX,c(accuracy(fc_arima_walmart_weekly_TX, test_weekly_TX)))
colnames(summary_weekly_TX) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_weekly_TX) <- c('Seasonal Naive Model for Weekly freq. of TX dataset','ETS Model for Weekly freq. of TX dataset','ARIMA Model for Weekly freq. of TX dataset')
summary_weekly_TX
knitr::kable(summary_weekly_TX)

summary_monthly_TX <- matrix(c(accuracy(fc_snaive_walmart_monthly_TX, test_monthly_TX)), ncol=5, byrow=TRUE)
summary_monthly_TX <- as.table(summary_monthly_TX)
summary_monthly_TX <- rbind(summary_monthly_TX,c(accuracy(fc_ets_walmart_monthly_TX, test_monthly_TX)))
summary_monthly_TX <- rbind(summary_monthly_TX,c(accuracy(fc_arima_walmart_monthly_TX, test_monthly_TX)))
colnames(summary_monthly_TX) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_monthly_TX) <- c('Seasonal Naive Model for Monthly freq. of TX dataset','ETS Model for Monthly freq. of TX dataset','ARIMA Model for Monthly freq. of TX dataset')
summary_monthly_TX
knitr::kable(summary_monthly_TX)




## Summary table for WI ----------------------------------------------------------

summary_weekly_WI <- matrix(c(accuracy(fc_snaive_walmart_weekly_WI, test_weekly_WI)), ncol=5, byrow=TRUE)
summary_weekly_WI <- as.table(summary_weekly_WI)
summary_weekly_WI <- rbind(summary_weekly_WI,c(accuracy(fc_ets_walmart_weekly_WI, test_weekly_WI)))
summary_weekly_WI <- rbind(summary_weekly_WI,c(accuracy(fc_arima_walmart_weekly_WI, test_weekly_WI)))
colnames(summary_weekly_WI) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_weekly_WI) <- c('Seasonal Naive Model for Weekly freq. of WI dataset','ETS Model for Weekly freq. of WI dataset','ARIMA Model for Weekly freq. of WI dataset')
summary_weekly_WI
knitr::kable(summary_weekly_WI)

summary_monthly_WI <- matrix(c(accuracy(fc_snaive_walmart_monthly_WI, test_monthly_WI)), ncol=5, byrow=TRUE)
summary_monthly_WI <- as.table(summary_monthly_WI)
summary_monthly_WI <- rbind(summary_monthly_WI,c(accuracy(fc_ets_walmart_monthly_WI, test_monthly_WI)))
summary_monthly_WI <- rbind(summary_monthly_WI,c(accuracy(fc_arima_walmart_monthly_WI, test_monthly_WI)))
colnames(summary_monthly_WI) <- c('ME','RMSE','MAE','MPE','MAPE')
rownames(summary_monthly_WI) <- c('Seasonal Naive Model for Monthly freq. of WI dataset','ETS Model for Monthly freq. of WI dataset','ARIMA Model for Monthly freq. of WI dataset')
summary_monthly_WI
knitr::kable(summary_monthly_WI)



# VAR Model ---------------------------------------------------------------
combine1 <- rbind(grpbydept, grpbycat) # 3*0, 7*0
combine2 <- rbind(combine1, grpbystore)
combine3 <- rbind(combine2, grpbystate)

combine3

# Transpose the combine dataset

transpose <- t(combine3)


#transpose = setNames(data.frame(t(combine[,-1])), combine[,1])
#transpose

transpose <- data.frame(transpose)
transpose

# Remove the last 28 entries from the calender csv so that we can merge calender and transposed dataset into one

calendar <- head(calendar, - 28)
calendar

# Merge the calender and transposed dataset
total <- cbind(transpose,calendar)
total <- data.frame(total)
#total <- data.frame(total)
total
colnames(total)

total_2 <- data.frame(total[,c(1:7)])
colnames(total_2)


dates <- seq(as.Date("2011-01-29"), length = 1941, by = "days")
data <- total_2

ts_data <- ts(monthly_mean,frequency = 12)
ts_data

train <- ts(ts_data[c(1:45),],frequency = 12)
train
test <- ts(ts_data[c(45:65),],frequency = 12)

diff_data <- diff(train,differences=1)
head(diff_data)

a <- VARselect(diff_data, type="both", season=12)
a
a$selection

VAR_model <- VAR(train, p=1, type="both", season=12)
VAR_model

library(forecast)
library(tseries)

forecast(VAR_model,h= 6)

forecast1 <- data.frame(forecast(VAR_model,n.ahead=21)$fcst)
forecast1

forecast1$FOODS_1.lower <- NULL 
forecast1$FOODS_1.upper <- NULL
forecast1$FOODS_1.CI <- NULL

forecast1$FOODS_2.lower <- NULL 
forecast1$FOODS_2.upper <- NULL 
forecast1$FOODS_2.CI <- NULL 

forecast1$FOODS_3.lower <- NULL 
forecast1$FOODS_3.upper <- NULL 
forecast1$FOODS_3.CI <- NULL 

forecast1$HOBBIES_1.lower <- NULL 
forecast1$HOBBIES_1.upper <- NULL 
forecast1$HOBBIES_1.CI <- NULL

forecast1$HOBBIES_2.lower <- NULL 
forecast1$HOBBIES_2.upper <- NULL 
forecast1$HOBBIES_2.CI <- NULL 

forecast1$HOUSEHOLD_1.lower <- NULL 
forecast1$HOUSEHOLD_1.upper <- NULL 
forecast1$HOUSEHOLD_1.CI <- NULL 

forecast1$HOUSEHOLD_2.lower <- NULL 
forecast1$HOUSEHOLD_2.upper <- NULL 
forecast1$HOUSEHOLD_2.CI <- NULL 


forecast1

summary(total_2$HOUSEHOLD_1)
summary(forecast1$HOUSEHOLD_1)

test

ts(forecast1,frequency = 12)

data.frame(test)$HOUSEHOLD_1

accuracy(forecast1$FOODS_1.fcst,data.frame(test)$FOODS_1)
accuracy(forecast1$FOODS_2.fcst,data.frame(test)$FOODS_2)
accuracy(forecast1$FOODS_3.fcst,data.frame(test)$FOODS_3)

accuracy(forecast1$HOBBIES_1.fcst,data.frame(test)$HOBBIES_1)
accuracy(forecast1$HOBBIES_2.fcst,data.frame(test)$HOBBIES_2)

accuracy(forecast1$HOUSEHOLD_1.fcst,data.frame(test)$HOUSEHOLD_1)
accuracy(forecast1$HOUSEHOLD_2.fcst,data.frame(test)$HOUSEHOLD_2)

me_foods1 <- mean(forecast1$FOODS_1.fcst-data.frame(test)$FOODS_1)
me_foods1

me_foods2 <- mean(forecast1$FOODS_2.fcst-data.frame(test)$FOODS_2)
me_foods2

me_household1 <- mean(forecast1$HOUSEHOLD_1.fcst-data.frame(test)$HOUSEHOLD_1)
me_household1


# Regression model - ELM Model --------------------------------------------

elmmodel <- elm(BJsalesInsample,xreg=as.matrix(window(BJsales.lead,1,120)))
BJELMForecast <- forecast(BJELM, h=30, xreg=as.matrix(BJsales.lead))

elmmodel <- elm(walmart_weekly_FOODS_1)
fc_elmmodel <- forecast(elmmodel, h=week_h)$mean
accuracy(fc_elmmodel, test_weekly_FOODS_1)

?elm
plot(fc_elmmodel)
elmmodel
str(total)

BJELMForecast <- forecast(BJELM, h=30, xreg=as.matrix(BJsales.lead))
walmart_weekly



# Forecast for 6 months ---------------------------------------------------

## Seasonal Naive Model for Weekly Series ----------------------------------

fcshort_snaive_walmart_weekly_FOODS_1 <- snaive(walmart_weekly_FOODS_1, h=weekshort_h)$mean

fcshort_snaive_walmart_weekly_FOODS_2 <- snaive(walmart_weekly_FOODS_2, h=weekshort_h)$mean

fcshort_snaive_walmart_weekly_FOODS_3 <- snaive(walmart_weekly_FOODS_3, h=weekshort_h)$mean

fcshort_snaive_walmart_weekly_HOBBIES_1 <- snaive(walmart_weekly_HOBBIES_1, h=weekshort_h)$mean
data.frame(fcshort_snaive_walmart_weekly_HOBBIES_1)


fcshort_snaive_walmart_weekly_HOBBIES_2 <- snaive(walmart_weekly_HOBBIES_2, h=weekshort_h)$mean

fcshort_snaive_walmart_weekly_HOUSEHOLD_1 <- snaive(walmart_weekly_HOUSEHOLD_1, h=weekshort_h)$mean

fcshort_snaive_walmart_weekly_HOUSEHOLD_2 <- snaive(walmart_weekly_HOUSEHOLD_2, h=weekshort_h)$mean

fcshort_snaive_walmart_weekly_FOODS <- snaive(walmart_weekly_FOODS, h=weekshort_h)$mean

fcshort_snaive_walmart_weekly_HOBBIES <- snaive(walmart_weekly_HOBBIES, h=weekshort_h)$mean

fcshort_snaive_walmart_weekly_HOUSEHOLD <- snaive(walmart_weekly_HOUSEHOLD, h=weekshort_h)$mean

fcshort_snaive_walmart_weekly_CA <- snaive(walmart_weekly_CA, h=weekshort_h)$mean

fcshort_snaive_walmart_weekly_TX <- snaive(walmart_weekly_TX, h=weekshort_h)$mean
fcshort_snaive_walmart_weekly_TX
summary(fcshort_snaive_walmart_weekly_TX)

fcshort_snaive_walmart_weekly_WI <- snaive(walmart_weekly_WI, h=weekshort_h)$mean

plot(fcshort_snaive_walmart_weekly_HOBBIES_1 <- snaive(walmart_weekly_HOBBIES_1, h=weekshort_h),main = "Forecasts from Seasonal Naive method of HOBBIES 1", xlab="Time frame", ylab="Sales volume")

?autoplot()
?plot

par(mfrow=c(1,1))

## Seasonal Naive Model for Monthly Series ----------------------------------

fcshort_snaive_walmart_monthly_FOODS_1 <- snaive(walmart_monthly_FOODS_1, h=month_h)$mean

fcshort_snaive_walmart_monthly_FOODS_2 <- snaive(walmart_monthly_FOODS_2, h=month_h)$mean

fcshort_snaive_walmart_monthly_FOODS_3 <- snaive(walmart_monthly_FOODS_3, h=month_h)$mean

fcshort_snaive_walmart_monthly_HOBBIES_1 <- snaive(walmart_monthly_HOBBIES_1, h=month_h)$mean

fcshort_snaive_walmart_monthly_HOBBIES_2 <- snaive(walmart_monthly_HOBBIES_2, h=month_h)$mean

fcshort_snaive_walmart_monthly_HOUSEHOLD_1 <- snaive(walmart_monthly_HOUSEHOLD_1, h=month_h)$mean

fcshort_snaive_walmart_monthly_HOUSEHOLD_2 <- snaive(walmart_monthly_HOUSEHOLD_2, h=month_h)$mean

fcshort_snaive_walmart_monthly_FOODS <- snaive(walmart_monthly_FOODS, h=month_h)$mean

fcshort_snaive_walmart_monthly_HOBBIES <- snaive(walmart_monthly_HOBBIES, h=month_h)$mean

fcshort_snaive_walmart_monthly_HOUSEHOLD <- snaive(walmart_monthly_HOUSEHOLD, h=month_h)$mean

fcshort_snaive_walmart_monthly_CA <- snaive(walmart_monthly_CA, h=month_h)$mean

fcshort_snaive_walmart_monthly_TX <- snaive(walmart_monthly_TX, h=month_h)$mean

fcshort_snaive_walmart_monthly_WI <- snaive(walmart_monthly_WI, h=month_h)$mean

## Seasonal ARIMA Model for Weekly series ----------------------------------


fcshort_arima_walmart_weekly_FOODS_1 <- forecast(arima_walmart_weekly_FOODS_1, h=weekshort_h)$mean
plot(fcshort_arima_walmart_weekly_FOODS_1 <- forecast(arima_walmart_weekly_FOODS_1, h=weekshort_h),main = "Forecasts from Seasonal ARIMA (0,1,2)[1](2,1,2)[52] with drift model of FOODS 1", xlab="Time frame", ylab="Sales volume")
arima_walmart_weekly_FOODS_1

data.frame(fcshort_arima_walmart_weekly_FOODS_1)
summary(fcshort_arima_walmart_weekly_FOODS_1)

fcshort_arima_walmart_weekly_FOODS_2 <- forecast(arima_walmart_weekly_FOODS_2, h=weekshort_h)$mean
plot(fcshort_arima_walmart_weekly_FOODS_2 <- forecast(arima_walmart_weekly_FOODS_2, h=weekshort_h),main = "Forecasts from Seasonal ARIMA (3,1,3)[1](2,1,0)[52] with drift model of FOODS 2", xlab="Time frame", ylab="Sales volume")
arima_walmart_weekly_FOODS_2
data.frame(fcshort_arima_walmart_weekly_FOODS_2)

fcshort_arima_walmart_weekly_FOODS_3 <- forecast(arima_walmart_weekly_FOODS_3, h=weekshort_h)$mean
plot(fcshort_arima_walmart_weekly_FOODS_3 <- forecast(arima_walmart_weekly_FOODS_3, h=weekshort_h),main = "Forecasts from Seasonal ARIMA (3,1,3)[1](2,0,0)[52] model of FOODS 3", xlab="Time frame", ylab="Sales volume")
arima_walmart_weekly_FOODS_3
data.frame(fcshort_arima_walmart_weekly_FOODS_3)


fcshort_arima_walmart_weekly_HOBBIES_1 <- forecast(arima_walmart_weekly_HOBBIES_1, h=weekshort_h)$mean


fcshort_arima_walmart_weekly_HOBBIES_2 <- forecast(arima_walmart_weekly_HOBBIES_2, h=weekshort_h)$mean


fcshort_arima_walmart_weekly_HOUSEHOLD_1 <- forecast(arima_walmart_weekly_HOUSEHOLD_1, h=weekshort_h)$mean
plot(fcshort_arima_walmart_weekly_HOUSEHOLD_1 <- forecast(arima_walmart_weekly_HOUSEHOLD_1, h=weekshort_h),main = "Forecasts from Seasonal ARIMA (0,1,3)[1](2,1,2)[52] with drift model of HOUSEHOLD 1", xlab="Time frame", ylab="Sales volume")
arima_walmart_weekly_HOUSEHOLD_1
data.frame(fcshort_arima_walmart_weekly_HOUSEHOLD_1)

fcshort_arima_walmart_weekly_HOUSEHOLD_2<- forecast(arima_walmart_weekly_HOUSEHOLD_2, h=weekshort_h)$mean
plot(fcshort_arima_walmart_weekly_HOUSEHOLD_2<- forecast(arima_walmart_weekly_HOUSEHOLD_2, h=weekshort_h),main = "Forecasts from Seasonal ARIMA (0,1,3)[1](2,0,0)[52] model of HOUSEHOLD 2", xlab="Time frame", ylab="Sales volume")
arima_walmart_weekly_HOUSEHOLD_2
data.frame(fcshort_arima_walmart_weekly_HOUSEHOLD_2)

fcshort_arima_walmart_weekly_FOODS<- forecast(arima_walmart_weekly_FOODS, h=weekshort_h)$mean


fcshort_arima_walmart_weekly_HOBBIES<- forecast(arima_walmart_weekly_HOBBIES, h=weekshort_h)$mean


fcshort_arima_walmart_weekly_HOUSEHOLD<- forecast(arima_walmart_weekly_HOUSEHOLD, h=weekshort_h)$mean


fcshort_arima_walmart_weekly_CA<- forecast(arima_walmart_weekly_CA, h=weekshort_h)$mean
summary(fcshort_arima_walmart_weekly_CA)

fcshort_arima_walmart_weekly_TX<- forecast(arima_walmart_weekly_TX, h=weekshort_h)$mean


fcshort_arima_walmart_weekly_WI<- forecast(arima_walmart_weekly_WI, h=weekshort_h)$mean
summary(fcshort_arima_walmart_weekly_WI)




