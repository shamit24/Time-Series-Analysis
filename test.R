getwd()
library(zoo)
library(tidyverse)

##importing data
Fed<- read.csv("FederalReserveInterestRates.csv", skip=5,as.is=TRUE)
Gold<- read.csv("GOLDmonthly_csv.csv",as.is=TRUE)
SP<- read.csv("S&Pdata_csv.csv",colClasses= c(NA,NA, rep("NULL",8)),as.is=TRUE)

##renaming column name in the Fed database
names(Fed) <- c("Date", "Interest")

##Converting Date from format to Date

Fed$Date<-paste(1, Fed$Date)
Gold$Date<-paste(1, Gold$Date)

SP$Date<-as.Date(SP$Date,"%Y-%m-%d")
Gold$Date<-as.Date(Gold$Date, "%d %Y-%m")

################################################################

Fed$Date<-as.Date(Fed$Date,"%d %b-%y")


#Gold$Date<-as.yearmon(Gold$Date,"%Y-%m")
#SP$Date<-substr(SP$Date,1,nchar(SP$Date)-3)
#SP$Date<-as.yearmon(SP$Date,"%Y-%m")
#Fed$Date<-as.yearmon(Fed$Date,"%b-%Y")

str(Fed)
str(Gold)
str(SP)


## MERGE
all<- merge(Gold, SP, by="Date")
all<- merge(all, Fed, by="Date")

class(all)
sapply(all, class)


####################################################################


plot(all)

plot(Price~Date, data = all)
plot(SP500~Date, data = all)
plot(Interests_rates~Date, data = all)


####################################################################

# Sort data by date column:
all2 <- all[order(all$Date),]
head(all2)
tail(all2)

#####################################################################

#Convert to time series:
 

library(dplyr)

str(all2)
colnames(all2)

nya <- all2
nya <- nya[,-1]  
colnames(nya)

nya2 <-ts(nya, frequency = 12, start = c(1969, 1))

summary(nya2)

plot(nya2, nc = 2, xlab = "")


####################################################################

# Transform non-stationery data into a stationery series for analysis:
# To address the trend component, take difference of the time series.

str(nya2)

nya2_diff <- diff(nya2)



# Plot to check data stationarity:
plot(nya2_diff)


#####################################################################
library(vars)

# VAR model implies that everything depends on everything.
# Fit a VAR model
# Decide optimal lag length as per AIC:
# An AR(p) model is an autoregressive model where specific lagged values of yt are used as predictor variables. 
# Lags are where results from one time period affect following periods.
# The value for "p" is called the order. For example, an AR(1) would be a "first order autoregressive process." The outcome variable in a first order AR process at some point in time t is related only to time periods that are one period apart (i.e. the value of the variable at t - 1). A second or third order AR process would be related to data two or three periods apart.
# The autocorrelation is the average correlation between all pairs of years that differ by 1, 2 years, and so on. The differences are referred to as lags.

library(vars)
varts1 <- VAR(nya2_diff, lag.max = 4, ic - "AIC")
summary(varts1)
plot(varts1)




## Positive correlation is a relationship between two variables where if one variable increases, the other one also increases. A positive correlation also exists in one decreases and the other also decreases.
## Negative correlation is a relationship between two variables in which one variable increases as the other decreases, and vice versa. In statistics, a perfect negative correlation is represented by the value -1, a 0 indicates no correlation, and a +1 indicates a perfect positive correlation


# From the model summary you can see Gold price is highly correlated with its own value at both lag 1 and lag2.
# Gold price is also statistically significant for Interest rate at lag 1.
# Similarly SP500 is high correlated with its own value for lag 1 only. No other correlations are statistically significant.
# Finally, Interest rate has significant impact because of Interest rateat lag 1 and lag 2. Also it is significantly correlated with gold price at lag 2. 


# Covariance is a measure of the joint variability of two random variables. The sign of the covariance therefore shows the tendency in the linear relationship between the variables.
# The magnitude of the covariance is not easy to interpret because it is not normalized and hence depends on the magnitudes of the variables.
# The normalized version of the covariance, the correlation coefficient, however, shows by its magnitude the strength of the linear relation.
# Our model's Covariance matrix shows positive covariance between gold price and interest rates, whereas negative covariane exists between gold price and SP500.
# SP500 shows positive covariance with interest rates.
# Interest rate shows positive covariance with itself, gold price and SP500 

# Our model's correlation matrix shows positive correlation b/w gold price and interest rate but negative correlation b/w gold price and SP500.
# Similarly SP500 shows positive correlation for interest rate.
# Interest rate shows positive correlation not only with itself but also with gold price and Interest rate.

# Also to test the results and to check the overall fit of the model, we can analyse the value of R-squared
# R-squared shows the proportion of variation explained by the estimated regression line.
# The smaller the variation, the better the regression model fits the sample data.
# R-squared always takes on a value b/w 0 and 1. The closer R-squared is to 1, the better the estimated regression equation fits or explain the relationship b/w variables.
# Adjusted R-squared is used in case of multiple regression analysis. R-squared increases as new independent variables are added to regression equation.
# For our model we can see the Adjusted R-squared is higher than zero for all three estimations. 


######################################################################

# Make prediction using model 


forecastts <- predict(varts1, n.ahead = 12, ci=0.95)
forecastts

plot(forecastts) #plot1
fanchart(forecastts) #plot2


##################################### The End ############################################