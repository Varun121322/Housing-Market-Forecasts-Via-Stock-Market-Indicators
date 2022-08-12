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

# Starting with 2006, because data in 2005 not complete
openingValues <- rep(0, 200)
closingValues <- rep(0, 200)
minimumValues <- rep(0, 200)
maximumValues <- rep(0, 200)
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

# Insert Heiken-Aishi
japandataFrameHeikenAishi <- japanCandleStickDataFrame %>% 
  mutate(newClosing = (openingValues + closingValues + minimumValues + maximumValues) / 4)
newOpening <- rep(0, 200)
increment <- 1
for(i in 2:140)
{
  newOpening[[increment]] <- 0.5 * (openingValues[[1]][[i - 1]] + closingValues[[1]][[i - 1]])
  increment <- increment + 1
}
newOpening <- as_tibble(newOpening) %>% 
  filter(newOpening != 0) %>% 
  slice(1:138)

newClosingValues <- as_tibble(newClosingValues) %>% 
  filter(newClosingValues != 0) %>% 
  slice(1:138)

newMaximumValues <- as_tibble(newMaximumValues) %>% 
  filter(newMaximumValues != 0) %>% 
  slice(1:138)

newMinimumValues <- as_tibble(newMinimumValues) %>% 
  filter(newMinimumValues != 0) %>% 
  slice(1:138)



newMaximumValues = rep(0, 180)
previousMaximumValues <- japandataFrameHeikenAishi$value.2
counter <- 1
for(i in 2:140)
{
  newMaximumValues[[counter]] <- max(previousMaximumValues[[i]], newOpeningValues[[i]], newClosingValues[[i]])
  counter <- counter + 1
}
newMinimumValues <- rep(0, 180)
previousMinimumValues <- japandataFrameHeikenAishi$value.3
index <- 1
for(i in 2:140)
{
  newMinimumValues[[index]] <- min(previousMinimumValues[[i]], newOpeningValues[[i]], newClosingValues[[i]])
  index <- index + 1
}
japanHeikenAishi <- japandataFrameHeikenAishi %>% 
  slice(1:141)


macdCollectedJapan <- as_tibble(MACD(closingValues, 4, 3, 4))

macdBearishJapan <- rep(0, 134)
macdBullishJapan <- rep(0, 134)
index <- 1

for(i in 7:137)
{
  if(macdCollectedJapan[[1]][[i]] >= macdCollectedJapan[[2]][[i]])
  {
    if(macdCollectedJapan[[1]][[i + 1]] <= macdCollectedJapan[[2]][[i + 1]])
    {
      macdBearishJapan[[index]] <- 1
    }
  }
  if(macdCollectedJapan[[1]][[i]] <= macdCollectedJapan[[2]][[i]])
  {
    if(macdCollectedJapan[[1]][[i + 1]] >= macdCollectedJapan[[2]][[i + 1]])
    {
      macdBullishJapan[[index]] <- 1
    }
  }
  index <- index + 1
}

macdBearishFreqJapan <- length(which(macdBearishJapan == 1))
macdBullishFreqJapan <- length(which(macdBullishJapan == 1))


counter <- 1
japanBearishIndexes <- rep(0, 12)
for(i in 1:134)
{
  if(macdBearishJapan[[i]] == 1)
  {
    japanBearishIndexes[[counter]] <- i
    counter <- counter + 1
  }
}  

counter <- 1
japanBullishIndexes <- rep(0, 11)
for(i in 1:134)
{
  if(macdBullishJapan[[i]] == 1)
  {
    japanBullishIndexes[[counter]] <- i
    counter <- counter + 1
  }
}

japanMACDBearishConsider <- rep(0, 12)
counter <- 1
for(i in 1:12)
{
  japanMACDBearishConsider[[counter]] <- japanHeikenAishi$newClosing[[japanBearishIndexes[[i]]]]
  counter <- counter + 1
}

statsigJapanMACDBearish <- wilcox.test(japanMACDBearishConsider, japanHeikenAishi$newClosing, mu = 0, alternative = "less")

japanMACDBullishConsider <- rep(0, 11)
counter <- 1
for(i in 1:11)
{
  japanMACDBullishConsider[[counter]] <- japanHeikenAishi$newClosing[[japanBullishIndexes[[i]]]]
  counter <- counter + 1
}


statsigJapanMACDBullish <- wilcox.test(japanMACDBullishConsider, japanHeikenAishi$newClosing, mu = 0, alternative = "greater")

  
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

# Germany Analysis
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

germanyHeikenAishi <- germanyDataFrameCandlestick %>% 
  mutate(newClosingGermany = (value + value.1 + value.2 + value.3) / 4)
newOpeningValuesGermany <- rep(0, 200)
increment <- 1
for(i in 2:143)
{
  newOpeningValuesGermany[[increment]] <- 0.5 * (openingValuesGermany[[1]][[i - 1]] + closingValuesGermany[[1]][[i - 1]])
  increment <- increment + 1
}
newOpeningValuesGermany <- as_data_frame(newOpeningValuesGermany) %>% 
  filter(newOpeningValuesGermany != 0) %>% 
  slice(1:141)

newClosingValuesGermany <- as_data_frame(closingValuesGermany) %>% 
  filter(value != 0) %>% 
  slice(1:141)

newMaximumValuesGermany <- as_data_frame(newMaximumValuesGermany) %>% 
  filter(newMaximumValuesGermany != 0) %>% 
  slice(1:141)

newMinimumValuesGermany <- as_tibble(newMinimumValuesGermany) %>% 
  filter(value != 0)



newMaximumValuesGermany <- rep(0, 180)
previousMaximumValues <- germanyDataFrameCandlestick$value.2
counter <- 1
for(i in 1:141)
{
  newMaximumValuesGermany[[counter]] <- max(previousMaximumValues[[i]], newOpeningValuesGermany[[1]][[i]], newClosingValuesGermany[[1]][[i]])
  counter <- counter + 1
}
newMinimumValuesGermany <- rep(0, 180)
previousMinimumValues <- germanyDataFrameCandlestick$value.3
index <- 1
for(i in 1:141)
{
  newMinimumValuesGermany[[index]] <- min(previousMinimumValues[[i]], newOpeningValuesGermany[[1]][[i]], newClosingValuesGermany[[1]][[i]])
  index <- index + 1
}

newClosingValuesGermany <- as_tibble(germanyHeikenAishi$newClosingGermany) %>% 
  slice(1:141)
germanyHeikenAishi = data.frame(newOpeningValuesGermany, newClosingValuesGermany, newMaximumValuesGermany, newMinimumValuesGermany$value)

macdCollectedGermany <- as_tibble(MACD(germanyHeikenAishi$newMinimumValuesGermany.value, 4, 3, 4))

macdBearishGermany <- rep(0, 130)
macdBullishGermany <- rep(0, 130)
index <- 1

for(i in 7:130)
{
  if(macdCollectedGermany[[1]][[i]] >= macdCollectedGermany[[2]][[i]])
  {
    if(macdCollectedGermany[[1]][[i + 1]] <= macdCollectedGermany[[2]][[i + 1]])
    {
      macdBearishGermany[[index]] <- 1
    }
  }
  if(macdCollectedGermany[[1]][[i]] <= macdCollectedGermany[[2]][[i]])
  {
    if(macdCollectedGermany[[1]][[i + 1]] >= macdCollectedGermany[[2]][[i + 1]])
    {
      macdBullishGermany[[index]] <- 1
    }
  }
  index <- index + 1
}

macdBearishFreqGermany <- length(which(macdBearishGermany == 1))
macdBullishFreqGermany <- length(which(macdBullishGermany == 1))

counter <- 1
germanyBearishIndexes <- rep(0, 11)
for(i in 1:130)
{
  if(macdBearishGermany[[i]] == 1)
  {
    germanyBearishIndexes[[counter]] <- i
    counter <- counter + 1
  }
}  

counter <- 1
germanyBullishIndexes <- rep(0, 11)
for(i in 1:130)
{
  if(macdBullishGermany[[i]] == 1)
  {
    germanyBullishIndexes[[counter]] <- i
    counter <- counter + 1
  }
}

germanyMACDBearishConsider <- rep(0, 11)
counter <- 1
for(i in 1:11)
{
  item <- germanyBearishIndexes[[i]]
  germanyMACDBearishConsider[[counter]] <- newClosingValuesGermany$value[[item]]
  counter <- counter + 1
}

statsigGermanyMACDBearish <- wilcox.test(germanyMACDBearishConsider, closingValuesGermany$value, mu = 0, alternative = "less")

germanyMACDBullishConsider <- rep(0, 11)
counter <- 1
for(i in 1:11)
{
  item <- germanyBullishIndexes[[i]]
  germanyMACDBullishConsider[[counter]] <- newClosingValuesGermany$value[[japanBullishIndexes[[i]]]]
  counter <- counter + 1
}


statSigGermanyMACDBullish <- wilcox.test(germanyMACDBullishConsider, closingValuesGermany$value, mu = 0, alternative = "greater")


# Canada Housing Market 
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

# Starting with 2006, because data in 2005 not complete
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
macdCollectedCanada <- as_tibble(MACD(newClosingValuesCanada, 4, 3, 4))

# Insert Heiken-Aishi
canadaHeikeinAishi <- canadaCandleStickDataFrame %>% 
  mutate(newClosingValuesCanada = (value + value.1 + value.2 + value.3) / 4)

newClosingCanada <- canadaHeikeinAishi$newClosingValuesCanada
newOpeningValuesCanada <- rep(0, 200)
increment <- 1
for(i in 2:140)
{
  newOpeningValuesCanada[[increment]] <- 0.5 * (openingValuesCanada[[1]][[i - 1]] + closingValuesCanada[[1]][[i - 1]])
  increment <- increment + 1
}
newOpeningValuesCanada <- as_tibble(newOpeningValuesCanada) %>% 
  filter(value != 0) %>% 
  slice(1:136)

newClosingValuesCanada <- as_tibble(canadaHeikeinAishi$newClosingValuesCanada) %>% 
  filter(value != 0) %>% 
  slice(1:136)

newMaximumValuesCanada <- as_tibble(newMaximumValuesCanada) %>% 
  filter(value != 0) %>% 
  slice(1:136)

newMinimumValuesCanada <- as_tibble(newMinimumValuesCanada) %>%
  slice(1:136)



newMaximumValuesCanada = rep(0, 180)
previousMaximumValuesCanada <- as_data_frame(canadaHeikeinAishi$value.2)
counter <- 1
for(i in 2:137)
{
  newMaximumValuesCanada[[counter]] <- max(previousMaximumValuesCanada[[1]][[i]], newOpeningValuesCanada[[1]][[i]], newClosingValuesCanada[[1]][[i]])
  counter <- counter + 1
}
newMinimumValuesCanada <- rep(0, 180)
previousMinimumValuesCanada <- as_tibble(canadaHeikeinAishi$value.3)
index <- 1
for(i in 2:137)
{
  newMinimumValuesCanada[[index]] <- min(previousMinimumValuesCanada[[1]][[i]], newOpeningValuesCanada$value[[i]], newClosingValuesCanada[[1]][[i]])
  index <- index + 1
}
canadaHeikeinAishi = data.frame(newOpeningValuesCanada, newClosingValuesCanada, newMaximumValuesCanada, newMinimumValuesCanada)



macdBearishCanada <- rep(0, 137)
macdBullishCanada <- rep(0, 137)
index <- 1

for(i in 7:136)
{
  if(macdCollectedCanada[[1]][[i]] >= macdCollectedCanada[[2]][[i]])
  {
    if(macdCollectedCanada[[1]][[i + 1]] <= macdCollectedCanada[[2]][[i + 1]])
    {
      macdBearishCanada [[index]] <- 1
    }
  }
  if(macdCollectedCanada[[1]][[i]] <= macdCollectedCanada[[2]][[i]])
  {
    if(macdCollectedCanada[[1]][[i + 1]] >= macdCollectedCanada [[2]][[i + 1]])
    {
      macdBullishCanada[[index]] <- 1
    }
  }
  index <- index + 1
  
}

macdBearishFreqCanada <- length(which(macdBearishCanada == 1))
macdBullishFreqCanada <- length(which(macdBullishCanada == 1))

counter <- 1
canadaBearishIndexes <- rep(0, 13)
for(i in 1:137)
{
  if(macdBearishCanada[[i]] == 1)
  {
    canadaBearishIndexes[[counter]] <- i
    counter <- counter + 1
  }
}  

counter <- 1
canadaBullishIndexes <- rep(0, 14)
for(i in 1:137)
{
  if(macdBullishCanada[[i]] == 1)
  {
    canadaBullishIndexes[[counter]] <- i
    counter <- counter + 1
  }
}

canadaMACDBearishConsider <- rep(0, 11)
counter <- 1
for(i in 1:13)
{
  item <- canadaBearishIndexes[[i]]
  canadaMACDBearishConsider[[counter]] <- newClosingValuesCanada$value[[item]]
  counter <- counter + 1
}

statsigCanadaMACDBearish <- wilcox.test(canadaMACDBearishConsider, closingValuesCanada$value, mu = 0, alternative = "less")

canadaMACDBullishConsider <- rep(0, 14)
counter <- 1
for(i in 1:14)
{
  item <- canadaBullishIndexes[[i]]
  canadaMACDBullishConsider[[counter]] <- newClosingValuesCanada$value[[item]]
  counter <- counter + 1
}

statSigCanadaMACDBullish <- wilcox.test(canadaMACDBullishConsider, closingValuesCanada$value, mu = 0, alternative = "greater")
