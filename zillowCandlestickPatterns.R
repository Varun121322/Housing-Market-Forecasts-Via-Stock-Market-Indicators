library(tidyverse)
library(primer.data)
library(lubridate)
library(skimr)
library(nycflights13)
library(gapminder)
library(fivethirtyeight)
library(quantmod)
library(dplyr)
library(tbl2xts)
library(ggthemes)
library(TTR)
library(data.table) 
library(ggplot2) 
library(scales) 
library(tidyquant)


# Candlestick Patterns


# Bearish Engulfing Recognition (Bearish)
bearishEngulfingRecognition <- function(openPrice, closePrice, highPrice, lowPrice) {
  pattern <- 0 * seq(1, nrow(openPrice), 1)
  for (i in c(2:nrow(openPrice))) {
    if (as.numeric(openPrice[i, 1]) > as.numeric(closePrice[i - 1, 1]) &&
        as.numeric(closePrice[i - 1, 1]) > as.numeric(openPrice[i - 1, 1]) &&
        as.numeric(openPrice[i - 1, 1]) > as.numeric(closePrice[i, 1])) {
      pattern[i - 1] = 1
    }
  }
  return(pattern)
  
}

# Bullish Engulfing Recognition (Bullish)
bullishEnglufingRecognition <- function(openPrice, close, high, low) {
  pattern <- 0 * seq(1, nrow(openPrice), 1)
  # starting days are marked as one
  for (i in c(2:nrow(openPrice))) {
    if (as.numeric(close[i, 1]) > as.numeric(openPrice[i - 1, 1]) &&
        as.numeric(openPrice[i - 1, 1]) > as.numeric(close[i - 1, 1]) &&
        as.numeric(close[i - 1, 1]) > as.numeric(openPrice[i, 1])) {
      pattern[i - 1] = 1
      
    }
  }
  return(pattern)
}

# Dark Cloud Cover Recognition (Bearish)
darkCloudCoverRecoginiton <- function(openingPrice, closingPrice, highPrice, lowPrice) {
  openingPriceMatrix <- as.matrix(openingPrice)
  highPriceMatrix <-as.matrix(highPrice)
  lowPriceMatrix <-as.matrix(lowPrice)
  closePriceMatrix <-as.matrix(closingPrice)
  nday1<- length(openingPriceMatrix)
  pattern <- 0*seq(1,nday1, 1)
  for (i in c(2:nday1)) {
    intermediate <-
      0.5 *( closePriceMatrix[i - 1] + openingPriceMatrix[i - 1])
    if (openingPriceMatrix[i] > highPriceMatrix[i - 1]&& highPriceMatrix[i-1]> closePriceMatrix[i]&& closePriceMatrix[i]> openingPriceMatrix[i-1] && closePriceMatrix[i]< intermediate && closePriceMatrix[i-1]> openingPriceMatrix[i-1]) {
      pattern[i - 1] = 1
    }
  }
  return(pattern)
}


# Hanging Man Recognition (Bearish)
hangingManRecogniton <- function(openingPrice, closingPrice, highPrice, lowPrice) {
  pattern <- 0 * seq(1, nrow(openingPrice), 1)
  # starting days are marked as one
  for (i in c(2:nrow(openingPrice))) {
    openingValue <- as.numeric(openingPrice[i, 1])
    closeValue<-as.numeric(closingPrice[i,1])
    highValue <- as.numeric(highPrice[i,1])
    lowValue <- as.numeric(lowPrice[i,1])
    if (TRUE &&
        as.numeric(openingPrice[i, 1]) > as.numeric(closingPrice[i, 1]) &&
        as.numeric(closingPrice[i, 1]) > as.numeric(lowPrice[i, 1])) {
      signalPresent<- TRUE
      
    }
    difference <-highValue-openingValue
    range<- (highValue-lowValue)
    
    if (difference> 0.01*range)
      {
      signalPresent<- FALSE
    }
    
    lowerShadow<- as.numeric(closingPrice[i,1])- as.numeric(lowPrice[i,1])
    realBody<- as.numeric(openingPrice[i,1])- as.numeric(closingPrice[i,1])
    if(lowerShadow< 2 * realBody){
      signalPresent <- FALSE
    }
    
    if(signalPresent){
      pattern[i] = 1
    }
  }
  return(pattern)
}

# Hammer Recognition(Bullish Engulfing)
hammerRecognition <- function(openingPrice, closingPrice, highPrice, lowPrice) {
  pattern <- 0 * seq(1, nrow(openingPrice), 1)
  # starting days are marked as one
  for (i in c(2:nrow(openingPrice))) {
    openingValue <- as.numeric(openingPrice[i, 1])
    closingValue <-as.numeric(closingPrice[i,1])
    highValue <- as.numeric(highPrice[i,1])
    lowValue <- as.numeric(lowPrice[i,1])
    if (TRUE &&
        as.numeric(openingPrice[i, 1]) < as.numeric(closingPrice[i, 1]) &&
        as.numeric(openingPrice[i, 1]) > as.numeric(lowPrice[i, 1])) {
      signalPresent<- TRUE
    }
    difference <-highValue - closingValue
    potiential <-highValue - lowValue
    if (difference > 0.01* potiential){
      signalPresent<- FALSE
    }
    lowerShadow<- as.numeric(openingPrice[i,1])- as.numeric(lowPrice[i,1])
    realBody<- abs(as.numeric(openingPrice[i,1])- as.numeric(closingPrice[i,1]))
    if(lowerShadow< 2 * realBody){
      signalPresent<-FALSE
    }
    if(signalPresent){
      pattern[i] = 1
    }
  }
  return(pattern)
}

# Identifying how often these patterns occur
# Then using that to obtain to statistical significance

# MACD Function in application
macdAnalysis <- function(stockPrice, shortTime, longTime, k)
{
  # Getting difference
  macdDifference <- EMA(stockPrice, shortTime) - EMA(stockPrice, longTime)
  # Getting signal line
  signalLine <- EMA(macdDifference, k)
  returnValue <- cbind(macdDifference, signalLine)
  colnames(returnValue) <- c("MACD", "signal")
  return(returnValue)
}

# Previewing MACD with Zillow Stock
# December 2018
AAPL <- getSymbols("AAPL", auto.assign = FALSE)
chartSeries(AAPL,
            subset='2019-05-25::2019-07-06',
            theme=chartTheme('white')) 
  addMACD(fast=12,slow=26,signal=9,type="EMA")


raw_data_zillow_weekly_trends <- read_csv("zillow_weakly_sales_price.csv")

clean_data_zillow_weekly_trends <- raw_data_zillow_weekly_trends %>% 
  drop_na()


getMeanValueOverIndicies <- function(lowerIndex, upperIndex)
{
  meanVal <- 0
  indexCounter <- 1
  weeklyAverages <- rep(0, 52)
  for(k in lowerIndex:upperIndex)
  {
    weeklyAverages[[indexCounter]] <- getMeanValueWeekly(k)
    indexCounter <- indexCounter + 1
  }
  return(mean(weeklyAverages))
}

getMeanValueWeekly <- function(index)
{
  sum <- 0
  for(d in 1:86)
  {
    sum <- sum + clean_data_zillow_weekly_trends[[index]][[d]]
  }
  av <- sum/86
  return(av)
}
# Average median housing price per week, with the first date being...
index <- 1
averages <- rep(0, 52)
count <- 1
for(i in 102:700)
{
  if(i %% 51 == 0)
  {
    averages[[index]] <- getMeanValueOverIndicies(i - 51, i)
    index <- index + 1
  }
}

yearlyAverages <- as_tibble(averages) 
x <- c(2007:2019)
yearlyAverages <- yearlyAverages %>% 
  filter(value != 0) %>%  
  mutate(year = 2017)

# Setting years
for(i in 1:12)
{
  yearlyAverages[[2]][[i]] = (2007 + i)
}

getMinOverIndices <- function(lowerIndex, upperIndex)
{
  minValue <- 1000000
  for(i in lowerIndex:upperIndex)
  {
    if(newSet[[1]][[i]] < minValue)
    {
      minValue <- newSet[[1]][[i]]
    }
  }
  return(minValue)
}

getMaxOverIndicies <- function(lowerIndex, upperIndex)
{
  maxVal <- 3
  for(i in lowerIndex:upperIndex)
  {
    if(newSet[[1]][[i]] > maxVal)
    {
      maxVal <- newSet[[1]][[i]]
    }
  }
  return(maxVal)
}

getPriceSum <- function(lowerInd, upperInd)
{
  sum <- 0
  for(i in lowerInd:upperInd)
  {
    sum <- sum + newSet[[1]][[i]]
  }
  return(sum)
}

# Clean the data, so we can graph some candlesticks
# 7-728
openingVals <- rep(0, 180)
closingVals <- rep(0, 180)
minimumVals <- rep(0, 180)
maximumVals <- rep(0, 180)

incrementer <- 1

newSet <- rep(0, 1000)
for(i in 7:727)
{
  newSet[[incrementer]] <- getMeanValueWeekly(i)
  incrementer <- incrementer + 1
}

newSet <- as_tibble(newSet) %>% 
  filter(value != 0) %>% 
  mutate(integerWeeks = 0)

counter <- 1

priceSum <- rep(0, 180)
for(k in 1:717)
{
  if(k %% 4 == 1)
  {
    openingVals[[counter]] <- newSet[[1]][[k]]
    closingVals[[counter]] <- newSet[[1]][[k + 4]]
    # Get min and max values
    minimumVals[[counter]] <- getMinOverIndices(k, k + 3)
    maximumVals[[counter]] <- getMaxOverIndicies(k, k + 3)
    priceSum[[counter]] <- getPriceSum(k, k + 3)
    counter <- counter + 1
  }
}
weeksIntoStudy <- c(1:180)
dataFrameCandleStick = data.frame(openingVals, closingVals, maximumVals, minimumVals, weeksIntoStudy, priceSum) %>% 
  filter(openingVals != 0)

openingPrices <- data.frame(dataFrameCandleStick$openingVals)
closingPrices <- data.frame(dataFrameCandleStick$closingVals)
maximumPrices <- data.frame(dataFrameCandleStick$maximumVals)
minimumPrices <- data.frame(dataFrameCandleStick$minimumVals)

# Candlestick Identifications
# P-values of candlesticks

bullishPattern <- bullishEnglufingRecognition(openingPrices, closingPrices, maximumPrices, minimumPrices)
hammerPattern <-hammerRecognition(openingPrices, closingPrices, maximumPrices, minimumPrices)
bearishPattern <- bearishEngulfingRecognition(openingPrices, closingPrices, maximumPrices, minimumPrices)
hangingManPattern <-hangingManRecogniton(openingPrices, closingPrices, maximumPrices, minimumPrices)
darkCloudPattern <- darkCloudCoverRecoginiton(openingPrices, closingPrices, maximumPrices, minimumPrices)


# Hanging Man Pattern
counter <- 1
temp <- rep(0, 81)
for(i in 1:length(hangingManPattern))
{
  if(darkCloudPattern[[i]] == 1)
  {
    temp[[counter]] <- 1
    counter <- counter + 1
  }
}

temp <- as_data_frame(temp) %>% 
  filter(value != 0)

getMaxHeikenAishi <- function(index)
{
  return(max(previousMaximumValues[[index]], newOpeningValues[[index]], newClosingValues[[index]]))
}

# Heiken-Aishi Candlestick Applications
dataFrameHeikenAishi <- dataFrameCandleStick %>% 
  mutate(newClosing = (openingVals + closingVals + minimumVals + maximumVals) / 4)

# Need new openingValues
newClosingValues <- dataFrameHeikenAishi$newClosing
newOpeningValues <- rep(0, 180)
increment <- 1
for(i in 2:180)
{
  newOpeningValues[[increment]] <- 0.5 * (openingVals[[i - 1]] + closingVals[[i - 1]])
  increment <- increment + 1
}
newMaximumValues = rep(0, 180)
previousMaximumValues = dataFrameHeikenAishi$maximumVals
counter <- 1
for(i in 1:180)
{
  newMaximumValues[[counter]] <- max(previousMaximumValues[[i]], newOpeningValues[[i]], newClosingValues[[i]])
  counter <- counter + 1
}
newMinimumValues <- rep(0, 180)
previousMinimumValues <- dataFrameHeikenAishi$minimumVals
index <- 1
for(i in 1:180)
{
  newMinimumValues[[index]] <- min(previousMinimumValues[[i]], newOpeningValues[[i]], newClosingValues[[i]])
  index <- index + 1
}

dataFrameHeikenAishi = data.frame(newOpeningValues, newClosingValues, newMaximumValues, newMinimumValues, weeksIntoStudy, priceSum) %>% 
  filter(openingVals != 0)

originalHeikenAishiPlot <- ggplot(data = dataFrameHeikenAishi, mapping = aes(x = weeksIntoStudy, y = priceSum)) +
  geom_candlestick(aes(open = newOpeningValues, high = newMaximumValues, low = newMinimumValues, close = newClosingValues)) +
  labs(x = "Weeks Since March 2008", y = "Average Median Price", title = "Heikin-Ashi Candlestick Chart Of Average Median House Sale Prices Since March 2008") + 
  scale_y_continuous(breaks = c(100000, 150000, 200000, 250000, 300000)) +
  theme_classic()

zillowopeningPrices <- data.frame(dataFrameHeikenAishi$newOpeningValues)
zillowclosingPrices <- data.frame(dataFrameHeikenAishi$newClosingValues)
zillowmaximumPrices <- data.frame(dataFrameHeikenAishi$newMaximumValues)
zillowminimumPrices <- data.frame(dataFrameHeikenAishi$newMinimumValues)

# Getting statistical significance
resultsDarkCloudCover <- darkCloudCoverRecoginiton(zillowopeningPrices, zillowclosingPrices, zillowmaximumPrices, zillowminimumPrices)

resultsBearishEngulging <- bearishEngulfingRecognition(zillowopeningPrices, zillowclosingPrices, zillowmaximumPrices, zillowminimumPrices)

resultsBullishEngulfing <- bullishEnglufingRecognition(zillowopeningPrices, zillowclosingPrices, zillowmaximumPrices, zillowminimumPrices)

resultsHammer <- hammerRecognition(zillowopeningPrices, zillowclosingPrices, zillowmaximumPrices, zillowminimumPrices)

resultsHangingMan <- hangingManRecogniton(zillowopeningPrices, zillowclosingPrices, zillowmaximumPrices, zillowminimumPrices)

# Statistical Significance

# Hanging Man Plot

# Bullish Engulfing, Hanging Man

resultsBullishEnglufingIndexTabulated <- rep(0, 180)
counting <- 1
for(i in 1:180)
{
  if(resultsBullishEngulfing[[i]] == 1)
  {
    # Getting correct indices where indicator occurs
    resultsBullishEnglufingIndexTabulated[[counting]] <- i
    counting <- counting + 1
  }
}

resultsHangingManTabulated <- rep(0, 180)
counter <- 1
for(i in 1:180)
{
  if(resultsHangingMan[[i]] == 1)
  {
    resultsHangingManTabulated[[counter]] <- i
    counter <- counter + 1
  }
}

resultsBullishEnglufingIndexTabulated <- as.data.frame(resultsBullishEnglufingIndexTabulated) %>% 
  filter(resultsBearishEnglufingIndexTabulated != 0)

dataFrameConsiderEngulfing <- dataFrameHeikenAishi[[1]][[179]]

statSigBullishEngulfing <- wilcox.test(dataFrameConsiderEngulfing, newDataFrameCandlestick$closingVals, mu = 0, alternative = "greater")
# P-Value of 0.05

resultsHangingManTabulated <- as.data.frame(resultsHangingManTabulated) %>% 
  filter(resultsHangingManTabulated != 0)
dataFrameConsideringHangingMan[[1]][[1]] <- dataFrameHeikenAishi[[1]][[29]]
dataFrameConsideringHangingMan[[1]][[2]] <- dataFrameHeikenAishi[[1]][[154]]
dataFrameConsideringHangingMan[[1]][[3]] <- dataFrameHeikenAishi[[1]][[169]]
dataFrameConsideringHangingMan[[1]][[4]] <- dataFrameHeikenAishi[[1]][[178]]
dataFrameConsideringHangingMan[[1]][[5]] <- dataFrameHeikenAishi[[1]][[179]]

dataFrameConsideringHangingMan <- as.data.frame(dataFrameConsideringHangingMan) %>% 
  filter(dataFrameConsideringHangingMan != 0) %>% 
  slice(1:5)

for(i in 1:5)
{
  dataFrameConsideringHangingMan[[1]][[i]] <- as.numeric(dataFrameConsideringHangingMan[[1]][[i]])
}
  

statSigHangingMan <- wilcox.test(dataFrameConsideringHangingMan[[1]], newDataFrameCandlestick$closingVals, mu = 0, alternative = "less")
# P-Value: 0.97






#

