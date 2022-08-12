library(tidyverse)
library(primer.data)
library(lubridate)
library(skimr)
library(nycflights13)
library(gapminder)
library(fivethirtyeight)
library(quantmod)
library(plotly)

library(TTR)


# Preview of RSI/MACD
Z <- getSymbols("Z", auto.assign = FALSE)
chartSeries(Z, 
            subset = '2020-03-01::2020-03-29',
            theme = chartTheme('white'))
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
dataFrameCandleStick = data.frame(openingVals, closingVals, maximumVals, minimumVals, weeksIntoStudy, priceSum) 

rsiPrices <- as_tibble(RSI(closingVals, n = 6))

newDataFrameCandlestick <- dataFrameCandleStick %>% 
  mutate(rsiPrices = rsiPrices$value)

# Now need to graph RSI and data concurrently to model trend
originalCandlestickPlot <- ggplot(data = dataFrameCandleStick, mapping = aes(x = weeksIntoStudy, y = priceSum)) +
  geom_candlestick(aes(open = openingVals, high = maximumVals, low = minimumVals, close = closingVals)) +
  labs(x = "Weeks Since March 2008", y = "Average Median Price", title = "Candlestick Chart Of Average Median House Sale Prices Since March 2008") + 
  theme_economist() 

rsiGraph <- ggplot(newDataFrameCandlestick, aes(x = weeksIntoStudy, y = rsiPrices)) +
  geom_line()

# 30-80 for thresholds?

# Index 39 RSI Bearish
rsiBullishShow <- rsiPrices %>% 
  slice(35:45) %>% 
  mutate(weeks = c(35:45)) %>% 
  mutate(opening = d$openingVals) %>% 
  mutate(closing = d$closingVals) %>% 
  mutate(maximum = d$maximumVals) %>% 
  mutate(minimum = d$minimumVals)

rsiBullishPlot1 <- ggplot(rsiBullishShow, aes(x = weeks, y = value)) +
  geom_line() +
  scale_x_continuous(breaks = c(35, 37, 39, 41, 43, 45)) +
  labs(x = "Months Since 2008", y = "RSI") +
  theme_classic()

rsiBullishPlot2 <- ggplot(data = rsiBullishShow, aes(x = weeks, y = closing)) +
  geom_candlestick(aes(open = opening, high = maximum, low = minimum, close = closing)) +
  labs(x = "", y = "Housing Prices", title = "RSI Bullish Reversal") +
  scale_x_continuous(breaks = c()) +
  theme_classic()

d <- dataFrameCandleStick %>% 
  slice(35:45)

rsiBearishShow <- rsiPrices %>% 
  slice(25:39) %>% 
  mutate(weeks = c(35:49)) %>% 
  mutate(opening = e$openingVals) %>% 
  mutate(closing = e$closingVals) %>% 
  mutate(maximum = e$maximumVals) %>% 
  mutate(minimum = e$minimumVals)



e <- dataFrameCandleStick %>% 
  slice(25:39)

rsiBearishPlot1 <- ggplot(rsiBearishShow, aes(x = weeks, y = value)) +
  geom_line() +
  scale_x_continuous(breaks = c(25, 27, 29, 31, 33, 35, 37, 39)) +
  labs(x = "Months Since 2008", y = "RSI") +
  theme_classic()

rsiBearishPlot2 <- ggplot(data = rsiBearishShow, aes(x = weeks, y = closing)) +
  geom_candlestick(aes(open = opening, high = maximum, low = minimum, close = closing)) +
  labs(x = "", y = "Housing Prices", title = "RSI Bearish Reversal", cex = 5) +
  scale_x_continuous(breaks = c()) +
  theme_classic()

p <- read_csv("p-vals.csv")


# Now will need to exhibit example of RSI being successful
# One example of the signal success is indices 52-53 
rsiExhibitedBullish <- newDataFrameCandlestick %>% 
  slice(35:75)

# Need to fix axis labelling, but indicator present
rsiBullishPlot <- ggplot(rsiExhibitedBullish, aes(x = weeksIntoStudy, y = closingVals)) +
  geom_candlestick(aes(open = openingVals, high = maximumVals, low = minimumVals, close = closingVals)) +
  geom_line(aes(y = rsiPrices)) 

# RSI Bearish (80-120)

rsiExhibitedBearish <- newDataFrameCandlestick %>% 
  slice(80:120)

rsiBearishPlot <- ggplot(rsiExhibitedBearish, aes(x = weeksIntoStudy)) +
  geom_line(aes(y = rsiPrices)) +
  geom_candlestick(aes(open = openingVals, high = maximumVals, low = minimumVals, close = closingVals))
  

rsiBearishPlot



# Now need to visualize

rsiOver <- length(which(rsiPrices > 85))
rsiUnder <- length(which(rsiPrices < 35))

rsiOverValsDataFrame <- rsiPrices %>% 
  filter(rsiPrices > 85)

rsiUnderValsDataFrame <- rsiPrices %>% 
  filter(rsiPrices < 35)

# Need to calculate further p-values, but this is a start.
# 2.2 * 10^-16
statSignificanceRSIBearish <- wilcox.test(rsiOverValsDataFrame$value, newDataFrameCandlestick$closingVals, mu = 0, alternative = "less")
statSignificanceRSIBullish <- wilcox.test(rsiUnderValsDataFrame$value, newDataFrameCandlestick$closingVals, mu = 0, alternative = "less")

# RSI Bearish Statistical Significance: 9.113 * 10^-10 
# RSI Bullish Statistical Significance: 2.2 * 10^-16


# 69-77 (Bearish)

# 26-34
exampleIMGBearishDataRSI <- dataFrameCandleStick %>% 
  mutate(RSI = rsiPrices$value) %>% 
  slice(69:77)

p5 <- ggplot(data = exampleIMGBearishDataRSI, aes(x = weeksIntoStudy))
p5 <- p5 + geom_line(aes(y = RSI), color = "black", size = 0.75)
p5 <- p5 + theme_classic()

p5Second <- ggplot(data = exampleIMGBearishDataRSI, aes(x = weeksIntoStudy, y = closingVals)) +
  geom_candlestick(aes(open = openingVals, high = maximumVals, low = minimumVals, close = closingVals)) +
  labs(title = "Example of RSI Bearish Reversal") +
  ylab("Average Median Housing Price") +
  xlab('') +
  scale_x_continuous(breaks = c(69, 71, 73, 75, 77)) +
  theme_classic()

exampleIMGBullishhDataRSI <- dataFrameCandleStick %>% 
  mutate(RSI = rsiPrices$value) %>% 
  slice(26:34)


p6 <- ggplot(data = exampleIMGBullishhDataRSI, aes(x = weeksIntoStudy))
p6 <- p6 + geom_line(aes(y = RSI), color = "black", size = 0.75)
p6 <- p6 + theme_classic()


p6Second <- ggplot(data = exampleIMGBullishhDataRSI, aes(x = weeksIntoStudy, y = closingVals)) +
  geom_candlestick(aes(open = openingVals, high = maximumVals, low = minimumVals, close = closingVals)) +
  labs(title = "Example of RSI Bullish Reversal") +
  ylab("Average Median Housing Price") +
  xlab('') +
  scale_x_continuous(breaks = c(26, 28, 30, 32, 34)) +
  theme_classic()
