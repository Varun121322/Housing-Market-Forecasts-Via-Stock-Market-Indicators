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

# Japan Analysis First
raw_data_japan <- read_csv("japanHousingPrices.csv")

# Basic Cleaning

clean_data_japan <- raw_data_japan %>% 
  mutate(housingPrice = QJPN628BIS * 1000)

# Now need to form candlesticks, but first ordering dates correctly
clean_data_japan <- as_tibble(clean_data_japan)
clean_data_japan[order(as.Date(clean_data_japan$DATE, format="%m/%d/%Y")),]

getMinOverIndices <- function(lowerIndex, upperIndex)
{
  minValue <- 1000000
  for(i in lowerIndex:upperIndex)
  {
    if(clean_data_japan[[3]][[i]] < minValue)
    {
      minValue <- clean_data_japan[[3]][[i]]
    }
  }
  return(minValue)
}

getMaxOverIndicies <- function(lowerIndex, upperIndex)
{
  maxVal <- 3
  for(i in lowerIndex:upperIndex)
  {
    if(clean_data_japan[[3]][[i]] > maxVal)
    {
      maxVal <- clean_data_japan[[3]][[i]]
    }
  }
  return(maxVal)
}

openingValues <- rep(0, 220)
closingValues <- rep(0, 220)
minimumValues <- rep(0, 220)
maximumValues <- rep(0, 220)
counter <- 1
for(i in 4:144)
{
  openingValues[[counter]] <- clean_data_japan[[3]][[i]]
  closingValues[[counter]] <- clean_data_japan[[3]][[i + 3]]
  minimumValues[[counter]] <- getMinOverIndices(i, i + 3)
  maximumValues[[counter]] <- getMaxOverIndicies(i, i + 3)
  i <- i + 4
  counter <- counter + 1
}

japanCandleStickDataFrame <- as_tibble(data.frame(openingValues, closingValues, maximumValues, minimumValues))
openingValues <- as_tibble(openingValues) %>% 
  filter(openingValues != 0)
closingValues <- as_tibble(closingValues) %>% 
  filter(closingValues != 0)
minimumValues <- as_tibble(minimumValues) %>% 
  filter(minimumValues != 0)
maximumValues <- as_tibble(maximumValues) %>% 
  filter(maximumValues != 0)

japanHeikenAishi <- japanHeikenAishi %>% 
  slice(1:141)

rsiPricesJapan <- as_tibble(RSI(japanHeikenAishi$newClosing, n = 12))

rsiBearishJapanFreq <- length(which(rsiPricesJapan > 80))
rsiBullishJapanFreq <- length(which(rsiPricesJapan < 15))

rsiBearishConsider <- rep(0, 26)
rsiBullishConsider <- rep(0, 44)
g <- 1
h <- 1
for(i in 13:141)
{
  if(rsiPricesJapan[[1]][[i]] > 80)
  {
    rsiBearishConsider[[g]] <- i
    g <- g + 1
  }
  if(rsiPricesJapan[[1]][[i]] < 15)
  {
    rsiBullishConsider[[h]] <- i
    h <- h + 1
  }
}

rsiBearishConsiderVals <- rep(0, 26)
rsiBUllishConsiderVals <- rep(0, 44)
c <- 1
c2 <- 1
for(i in 1:44)
{
  if(i < 27)
  {
    rsiBearishConsiderVals[[c]] <- rsiBearishConsider[[i]]
  }
  rsiBUllishConsiderVals[[c2]] <- rsiBullishConsider[[i]]
}

statSignificanceRSIBearishJapan <- wilcox.test(rsiBearishConsiderVals, japanHeikenAishi$newClosing, mu = 0, alternative = "less")
statSignificanceRSIBullishJapan <- wilcox.test(rsiBUllishConsiderVals, japanHeikenAishi$newClosing, mu = 0, alternative = "greater")


# Germany Analysis 

getMinOverIndicesGermany <- function(lowerIndex, upperIndex)
{
  minValue <- 1000000
  for(i in lowerIndex:upperIndex)
  {
    if(clean_data_germany[[3]][[i]] < minValue)
    {
      minValue <- clean_data_germany[[3]][[i]]
    }
  }
  return(minValue)
}

getMaxOverIndiciesGermany <- function(lowerIndex, upperIndex)
{
  maxVal <- 3
  for(i in lowerIndex:upperIndex)
  {
    if(clean_data_germany[[3]][[i]] > maxVal)
    {
      maxVal <- clean_data_germany[[3]][[i]]
    }
  }
  return(maxVal)
}

raw_data_germany <- read_csv("germanyHousingPricesFinal.csv")

# Basic Cleaning

clean_data_germany <- raw_data_germany %>% 
  mutate(housingPrice = QDER628BIS * 1000)

openingValuesGermany <- rep(0, 150)
closingValuesGermany <- rep(0, 150)
minimumValuesGermany <- rep(0, 150)
maximumValuesGermany <- rep(0, 150)
counter2 <- 1
for(i in 1:144)
{
  openingValuesGermany[[counter2]] <- clean_data_germany[[3]][[i]]
  closingValuesGermany[[counter2]] <- clean_data_germany[[3]][[i + 3]]
  minimumValuesGermany[[counter2]] <- getMinOverIndicesGermany(i, i + 3)
  maximumValuesGermany[[counter2]] <- getMaxOverIndiciesGermany(i, i + 3)
  i <- i + 4
  counter2 <- counter2 + 1
}

germanyDataFrameCandlestick <- as_tibble(data.frame(openingValuesGermany, closingValuesGermany, maximumValuesGermany, minimumValuesGermany))
openingValuesGermany <- as_tibble(openingValuesGermany) %>% 
  filter(openingValuesGermany != 0)
closingValuesGermany <- as_tibble(closingValuesGermany) %>% 
  filter(closingValuesGermany != 0)
minimumValuesGermany <- as_tibble(minimumValuesGermany) %>% 
  filter(minimumValuesGermany != 0)
maximumValuesGermany <- as_tibble(maximumValuesGermany) %>% 
  filter(maximumValuesGermany != 0)

germanyHeikenAishi <- germanyHeikenAishi %>% 
  slice(1:141)

rsiPricesGermany <- as_tibble(RSI(germanyHeikenAishi$value.1, n = 12))

rsiBearishGermanyFreq <- length(which(rsiPricesGermany > 75))
rsiBullishGermanyFreq <- length(which(rsiPricesGermany < 20))

rsiBearishConsiderGermany <- rep(0, 29)
rsiBullishConsiderGermany <- rep(0, 38)
g <- 1
h <- 1
for(i in 13:141)
{
  if(rsiPricesGermany[[1]][[i]] > 80)
  {
    rsiBearishConsiderGermany[[g]] <- i
    g <- g + 1
  }
  if(rsiPricesGermany[[1]][[i]] < 15)
  {
    rsiBullishConsiderGermany[[h]] <- i
    h <- h + 1
  }
}

rsiBearishConsiderValsGermany <- rep(0, 29)
rsiBullishConsiderValsGermany <- rep(0, 38)

c <- 1
c2 <- 1
for(i in 1:38)
{
  if(i < 30)
  {
    rsiBearishConsiderValsGermany[[c]] <- rsiBearishConsiderGermany[[i]]
  }
  rsiBullishConsiderValsGermany[[c2]] <- rsiBullishConsiderGermany[[i]]
}

statSignificanceRSIBearishGermany <- wilcox.test(rsiBearishConsiderValsGermany, germanyHeikenAishi$value.1, mu = 0, alternative = "less")
statSignificanceRSIBullishGermany <- wilcox.test(rsiBullishConsiderValsGermany, germanyHeikenAishi$value.1, mu = 0, alternative = "greater")

rsiCollectedGermany <- as_tibble(RSI(closingValuesGermany, 9))


# Canada Analysis
raw_data_canada <- read_csv("canadaHousingPricesFinal.csv")

# Basic Cleaning

clean_data_canada <- raw_data_canada %>% 
  mutate(housingPrice = QCAR628BIS * 1000)

# Now need to form candlesticks, but first ordering dates correctly
clean_data_canada <- as_tibble(clean_data_canada)
clean_data_canada[order(as.Date(clean_data_canada$DATE, format="%m/%d/%Y")),]

getMinOverIndicesCanada <- function(lowerIndex, upperIndex)
{
  minValue <- 1000000
  for(i in lowerIndex:upperIndex)
  {
    if(clean_data_canada[[3]][[i]] < minValue)
    {
      minValue <- clean_data_canada[[3]][[i]]
    }
  }
  return(minValue)
}

getMaxOverIndiciesCanada <- function(lowerIndex, upperIndex)
{
  maxVal <- 3
  for(i in lowerIndex:upperIndex)
  {
    if(clean_data_canada[[3]][[i]] > maxVal)
    {
      maxVal <- clean_data_canada[[3]][[i]]
    }
  }
  return(maxVal)
}

openingValuesCanada <- rep(0, 220)
closingValuesCanada <- rep(0, 220)
minimumValuesCanada <- rep(0, 220)
maximumValuesCanada <- rep(0, 220)
counter <- 1
for(i in 1:144)
{
  openingValuesCanada[[counter]] <- clean_data_canada[[3]][[i]]
  closingValuesCanada[[counter]] <- clean_data_canada[[3]][[i + 3]]
  minimumValuesCanada[[counter]] <- getMinOverIndicesCanada(i, i + 3)
  maximumValuesCanada[[counter]] <- getMaxOverIndiciesCanada(i, i + 3)
  i <- i + 4
  counter <- counter + 1
}

canadaCandleStickDataFrame <- as_tibble(data.frame(openingValuesCanada, closingValuesCanada, maximumValuesCanada, minimumValuesCanada))
openingValuesCanada <- as_tibble(openingValuesCanada) %>% 
  filter(openingValuesCanada != 0)
closingValuesCanada <- as_tibble(closingValuesCanada) %>% 
  filter(closingValuesCanada != 0)
minimumValuesCanada <- as_tibble(minimumValuesCanada) %>% 
  filter(minimumValuesCanada != 0)
maximumValuesCanada <- as_tibble(maximumValuesCanada) %>% 
  filter(maximumValuesCanada != 0)

canadaHeikeinAishi <- canadaHeikeinAishi %>% 
  slice(1:136)

rsiPricesCanada <- as_tibble(RSI(canadaHeikeinAishi$value.1, n = 12))

rsiBearishCanadaFreq <- length(which(rsiPricesCanada > 95))
rsiBullishCanadaFreq <- length(which(rsiPricesCanada < 55))

rsiBearishConsiderCanada <- rep(0, 31)
rsiBullishConsiderCanada <- rep(0, 24)
g <- 1
h <- 1
for(i in 13:136)
{
  if(rsiPricesCanada[[1]][[i]] > 95)
  {
    rsiBearishConsiderCanada[[g]] <- i
    g <- g + 1
  }
  if(rsiPricesCanada[[1]][[i]] < 55)
  {
    rsiBullishConsiderCanada[[h]] <- i
    h <- h + 1
  }
}

rsiBearishConsiderValsCanada <- rep(0, 31)
rsiBUllishConsiderValsCanada <- rep(0, 24)
c <- 1
c2 <- 1
for(i in 1:31)
{
  if(i < 25)
  {
    rsiBUllishConsiderValsCanada[[c]] <- rsiBullishConsiderCanada[[i]]
  }
  rsiBearishConsiderValsCanada[[c2]] <- rsiBearishConsiderCanada[[i]]
}

statSignificanceRSIBearishJCanada <- wilcox.test(rsiBearishConsiderValsCanada, canadaHeikeinAishi$value.1, mu = 0, alternative = "less")
statSignificanceRSIBullishCanada <- wilcox.test(rsiBUllishConsiderValsCanada, canadaHeikeinAishi$value.1, mu = 0, alternative = "greater")

rsiCollectedCanada <- as_tibble(RSI(closingValuesCanada, n = 9))
rsiBearishCanada <- rep(0, 135)
rsiBullishCanada <- rep(0, 135)
for(i in 10:140)
{
  if(rsiCollectedGermany[[1]][[i]] < 80 & rsiCollectedGermany[[1]][[i + 1]] > 80)
  {
    rsiBearishCanada[[counter]] <- 1
  }
  if(rsiCollectedGermany[[1]][[i]] > 45 & rsiCollectedGermany[[1]][[i + 1]] < 45)
  {
    rsiBullishCanada[[counter]] <- 1
  }
  counter <- counter + 1
}

rsiBearishCanadaFreq <- length(which(rsiBearishCanada == 1))
rsiBullishCanadaFreq <- length(which(rsiBullishCanada == 1))

