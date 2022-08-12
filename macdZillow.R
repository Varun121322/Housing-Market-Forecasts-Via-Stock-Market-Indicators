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
library(TTR)
library(tidyquant)
library(plotly)
library(ggplot2)
library(xts)

#

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

originalCandlestickPlot <- ggplot(data = dataFrameCandleStick, mapping = aes(x = weeksIntoStudy, y = priceSum)) +
  geom_candlestick(aes(open = openingVals, high = maximumVals, low = minimumVals, close = closingVals)) +
  labs(x = "Weeks Since March 2008", y = "Average Median Price", title = "Candlestick Chart Of Average Median House Sale Prices Since March 2008") + 
  theme_classic()

macdPlot <- ggplot(data = macdCollectedPrices, aes(x = weeks, y = macdPlot))

tibbleCandleStick = as_tibble(openingVals, closingVals, maximumVals, minimumVals, weeksIntoStudy, priceSum) %>% 
  filter(openingVals != 0)

macdCollectedPrices <- MACD(closingVals, 4, 12, 4)

macdCollectedPrices <- as.tibble(macdCollectedPrices) %>% 
  mutate(weeks = c(1:180))

macdSignalLine <- ggplot(data = macdCollectedPrices, aes(x = weeks)) +
  geom_line(aes(x = weeks, y = MACD))

macdLine <- ggplot(data = macdCollectedPrices, aes(x = weeks, y = MACD)) +
  geom_line()

dataFrameCandleStick <- dataFrameCandleStick %>% 
  mutate(MACD = macdCollectedPrices$macd) %>% 
  mutate(macdSignal = macdCollectedPrices$signal) 


# One example of Bearish Reversal is index 59-62 (For Graphing Purposes)

# One example of Bullish Reversal 70-8[0] (For Graphing Purposes)
# Now need to visualize that (and other potiential examples in a graph)

# exhibitedGraph <- ggplot(,(x = weeksIntoStudy, y = priceSum))


# Now will need to graph MACD, signal, and identify trends

# Will need to dynamically identify indices with trends, but for now will just show exhibited processes.

# Exhibited Bullish 

desiredExhibit <- dataFrameCandleStick %>% 
  slice(45:55)
pricingPlotBullish <- ggplot(desiredExhibit, aes(x = weeksIntoStudy, y = closingVals)) +
  geom_candlestick(aes(open = openingVals, high = maximumVals, low = minimumVals, close = closingVals)) +
  geom_line(aes(y = MACD), color = "green", size = 1) +
  geom_line(aes(y = macdSignal), color = "red", size = 0.5) +
  labs(title = "Example of MACD Bullish Reversal", 
       subtitle = "Black: MACD Red: Signal Line") +
  xlab('Months Since 2008') + 
  ylab('Average Median Housing Price') +
theme_economist()



p1 <- ggplot(data = desiredExhibit, aes(x = weeksIntoStudy))
p1 <- p1 + geom_line(aes(y = MACD), color = "black", size = 0.5)
p1 <- p1 + geom_line(aes(y = macdSignal), color = "red", size = 0.5)
p1 <- p1 + xlab("Months Since 2008")
p1 <- p1 + ylab("")
p1 <- p1 + scale_x_continuous(breaks = c(46, 48, 50, 52, 54))
p1 <- p1 + theme_classic()

p1Second <- ggplot(data = desiredExhibit, aes(x = weeksIntoStudy, y = closingVals)) +
  geom_candlestick(aes(open = openingVals, high = maximumVals, low = minimumVals, close = closingVals)) +
  labs(title = "Example of MACD Bullish Reversal", subtitle = "Black: MACD Red: Signal Line") +
  ylab("Average Median Housing Price") +
  xlab('') +
  theme_classic()

subplot(p1, p1Second, nrows = 2, shareX = T, heights = c(0.7, 0.3))


# Exhibited Bullish
desiredBearish <- dataFrameCandleStick %>% 
  mutate(MACD = macdCollectedPrices$macd) %>% 
  mutate(macdSignal = macdCollectedPrices$signal) %>% 
  slice(67:77)


  

pricingPlotBearish <- ggplot(desiredBearish, aes(x = weeksIntoStudy,y = closingVals)) +
  geom_candlestick(aes(open = openingVals, high = maximumVals, low = minimumVals, close = closingVals)) +
  geom_line(aes(y = MACD)) +
  geom_line(aes(y = macdSignal)) +
  labs(title = "Example of MACD Bearish Reversal", 
       subtitle = "Black: MACD Red: Signal Line") +
  xlab('Months Since 2008') + 
  ylab('Average Median Housing Price') +
  scale_y_continuous(breaks = seq(250, 400000, 25000)) +
theme_economist()

p2 <- ggplot(data = desiredBearish, aes(x = weeksIntoStudy))
p2 <- p2 + geom_line(aes(y = MACD), color = "black", size = 0.75)
p2 <- p2 +  geom_line(aes(y = macdSignal), color = "red", size = 0.75)
p2 <- p2 + xlab("Months Since 2008")
p2 <- p2 + ylab('')
p2 <- p2 + scale_x_continuous(breaks = seq(67, 77, 2))
p2 <- p2 + theme_classic()

p2Second <- p1Second <- ggplot(data = desiredBearish, aes(x = weeksIntoStudy, y = closingVals)) +
  geom_candlestick(aes(open = openingVals, high = maximumVals, low = minimumVals, close = closingVals)) +
  labs(title = "Example of MACD Bearish Reversal", subtitle = "Black: MACD Red: Signal Line") +
  ylab("Average Median Housing Price") +
  xlab('') +
  scale_x_continuous(breaks = seq(67, 77, 2)) +
  theme_classic()

# Indicators identification

macdBearish <- rep(0, 180)
macdBullish <- rep(0, 180)
index <- 1

for(i in 15:179)
{
  if(macdCollectedPrices[[1]][[i]] >= macdCollectedPrices[[2]][[i]])
  {
    if(macdCollectedPrices[[1]][[i + 1]] <= macdCollectedPrices[[2]][[i + 1]])
    {
      macdBearish[[index]] <- 1
    }
  }
  if(macdCollectedPrices[[1]][[i]] <= macdCollectedPrices[[2]][[i]])
  {
    if(macdCollectedPrices[[1]][[i + 1]] >= macdCollectedPrices[[2]][[i + 1]])
    {
      macdBullish[[index]] <- 1
    }
  }
  index <- index + 1
  
}

macdBearishFreq <- length(which(macdBearish == 1))
macdBullishFreq <- length(which(macdBullish == 1))

macdBearishTabulated <- rep(0, 180)
counter <- 1
for(i in 1:180)
{
  if(macdBearish[[i]] == 1)
  {
    macdBearishTabluated[[counter]] <- i
    counter <- counter + 1
  }
}
macdBearishConsider <- rep(0, 50)
for(i in 1:14)
{
  macdBearishConsider[[i]] <- macdBearishTabluated[[i]]
}

macdBearishConsider %>% as.data.frame(macdBearishConsider) %>% 
  slice(1:14)

newMACDBearishConsider <- rep(0, 50)
increase <- 1
for(i in 1:14)
{
  newMACDBearishConsider[[increase]] <- dataFrameHeikenAishi[[1]][[macdBearishConsider[[i]]]]
  increase <- increase + 1
}

macdBullishTabulated <- rep(0, 180)
inc <- 1
for(i in 1:180)
{
  if(macdBullish[[i]] == 1)
  {
    macdBullishTabulated[[inc]] <- i
    inc <- inc + 1
  }
}
macdBullishConsider <- rep(0, 50)
for(i in 1:13)
{
  macdBullishConsider[[i]] <- macdBullishTabulated[[i]]
}

macdBullishConsider %>% as.data.frame(macdBullishConsider) %>% 
  slice(1:13)

newMACDBullishConsider <- rep(0, 50)
increm <- 1
for(i in 1:13)
{
  newMACDBullishConsider[[increase]] <- dataFrameHeikenAishi[[1]][[macdBullishConsider[[i]]]]
  increm <- increm + 1
}






statSignificanceMACDBearish <- wilcox.test(newMACDBearishConsider, newDataFrameCandlestick$closingVals, mu = 0, alternative = "less")
# P-Value 2.2 * 10^-16
statSignificanceMACDBullish <- wilcox.test(newMACDBearishConsider, newDataFrameCandlestick$closingVals, mu = 0, alternative = "greater")
# P-Value of 0.999





# 23-29 for bullish
exampleIMGBullishData <- dataFrameCandleStick %>% 
  mutate(MACD = macdCollectedPrices$macd) %>% 
  mutate(macdSignal = macdCollectedPrices$signal) %>% 
  slice(23:31)
p3 <- ggplot(data = exampleIMGBullishData, aes(x = weeksIntoStudy))
p3 <- p3 + geom_line(aes(y = MACD), color = "black", size = 0.75)
p3 <- p3 +  geom_line(aes(y = macdSignal), color = "red", size = 0.75)
p3 <- p3 + theme_classic()


p3Second <- ggplot(data = exampleIMGBullishData, aes(x = weeksIntoStudy, y = closingVals)) +
  geom_candlestick(aes(open = openingVals, high = maximumVals, low = minimumVals, close = closingVals)) +
  labs(title = "Example of MACD Bullish Reversal", subtitle = "Black: MACD Red: Signal Line") +
  ylab("Average Median Housing Price") +
  xlab('') +
  scale_x_continuous(breaks = c(23, 25, 27, 29, 31)) +
  theme_classic()



# 149-156 for bearish
exampleIMGBearishData <- dataFrameCandleStick %>% 
  mutate(MACD = macdCollectedPrices$macd) %>% 
  mutate(macdSignal = macdCollectedPrices$signal) %>% 
  slice(135:143)

p4 <- ggplot(data = exampleIMGBearishData, aes(x = weeksIntoStudy))
p4 <- p4 + geom_line(aes(y = MACD), color = "black", size = 0.75)
p4 <- p4 +  geom_line(aes(y = macdSignal), color = "red", size = 0.75)
p4 <- p4 + theme_classic()

p4Second <- ggplot(data = exampleIMGBearishData, aes(x = weeksIntoStudy, y = closingVals)) +
  geom_candlestick(aes(open = openingVals, high = maximumVals, low = minimumVals, close = closingVals)) +
  labs(title = "Example of MACD Bearish Reversal", subtitle = "Black: MACD Red: Signal Line") +
  ylab("Average Median Housing Price") +
  xlab('') +
  scale_x_continuous(breaks = c(135, 137, 139, 141, 143)) +
  theme_classic()


result <- wilcox.test(macdCollectedPrices$MACD, macdCollectedPrices$signal, mu = 0, alternative = "less")
