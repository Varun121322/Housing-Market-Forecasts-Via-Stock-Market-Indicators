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

# Got data points correctly, but need to make graph look better
vectorForm <- c(2007:2018)
vectorForm <- as.character(vectorForm)
trendsPlot <- ggplot(data = yearlyAverages, mapping = aes(x = year, y = value)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Year", y = "Average Annual Value of House Sales",
       title = "Average Annual Value of House Sale Values Over Time") +
  theme_classic() +
  scale_x_continuous(c(2008,2019), n.breaks = 11) 


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
  labs(x = "Months Since March 2008", y = "Average Median Price", title = "Candlestick Chart Of Average Median House Sale Prices Since March 2008") + 
  theme_economist()

rsiSignalGraph <- ggplot(rsiPrices, aes(x = weeksIntoStudy, y = rsiPrices)) +
  geom_line()

#








