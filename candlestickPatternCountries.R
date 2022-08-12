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

# Candlestick Pattern Recognition
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


# Japan Analysis
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

resultsDarkCloudCover <- darkCloudCoverRecoginiton(openingValues, closingValues, maximumValues, minimumValues)
resultsBearishEngulging <- bearishEngulfingRecognition(openingValues, closingValues, maximumValues, minimumValues)
resultsBullishEngulfing <- bullishEnglufingRecognition(openingValues, closingValues, maximumValues, minimumValues)
resultsHammer <- hammerRecognition(openingValues, closingValues, maximumValues, minimumValues)
resultsHangingMan <- hangingManRecogniton(openingValues, closingValues, maximumValues, minimumValues)

freqBearishEngulfingJapan <- length(which(resultsBearishEngulging == 1))
freqBullishEngulfingJapan <- length(which(resultsBullishEngulfing == 1))
freqHammerJapan <- length(which(resultsHammer == 1))
freqHangingManJapan <- length(which(resultsHangingMan == 1))

# Bearish Engulfing Signiificance
indicies <- rep(0, 1)
counter <- 1
for(i in 1:141)
{
  if(resultsBearishEngulging[[i]] == 1)
  {
    indicies[[counter]] <- i
    counter <- counter + 1
  }
}

newJapanBearish <- japanCandleStickDataFrame[[2]][[21]]

statSigJapanBearishEngulfing <- wilcox.test(newJapanBearish, japanCandleStickDataFrame$closingValues, mu = 0, alternative = "less")

# Bullish Engulfing Significance
indiciesBullishEngulfing <- rep(0, 3)
counter <- 1
for(i in 1:141)
{
  if(resultsBearishEngulging[[i]] == 1)
  {
    indiciesBullishEngulfing[[counter]] <- i
    counter <- counter + 1
  }
}
bullishJapanConsider <- rep(0, 3)
count <- 1
for(i in 1:3)
{
  bullishJapanConsider[[count]] <- indiciesBullishEngulfing[[i]]
  count <- count + 1
}

statSigJapanBullishhEngulfing <- wilcox.test(bullishJapanConsider, japanCandleStickDataFrame$closingValues, mu = 0, alternative = "greater")

# Hammer Significance
indicies <- rep(0, 1)
counter <- 1
for(i in 1:141)
{
  if(resultsHammer[[i]] == 1)
  {
    indicies[[counter]] <- i
    counter <- counter + 1
  }
}

hammerConsider <- japanCandleStickDataFrame[[2]][[135]]

statSigJapanHammer <- wilcox.test(hammerConsider, japanCandleStickDataFrame$closingValues, mu = 0, alternative = "less")

# Hanging Man Significance
indicies <- rep(0, 1)
counter <- 1
for(i in 1:141)
{
  if(resultsHangingMan[[i]] == 1)
  {
    indicies[[counter]] <- i
    counter <- counter + 1
  }
}

hangingManConsider <- japanCandleStickDataFrame[[2]][[138]]

statSigJapanHangingMan <- wilcox.test(hangingManConsider, japanCandleStickDataFrame$closingValues, mu = 0, alternative = "less")
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

resultsDarkCloudCoverGermany <- darkCloudCoverRecoginiton(openingValuesGermany, closingValuesGermany, maximumValuesGermany, minimumValuesGermany)
resultsBearishEngulgingGermany <- bearishEngulfingRecognition(openingValuesGermany, closingValuesGermany, maximumValuesGermany, minimumValuesGermany)
resultsBullishEngulfingGermany <- bullishEnglufingRecognition(openingValuesGermany, closingValuesGermany, maximumValuesGermany, minimumValuesGermany)
resultsHammerGermany <- hammerRecognition(openingValuesGermany, closingValuesGermany, maximumValuesGermany, minimumValuesGermany)
resultsHangingManGermany <- hangingManRecogniton(openingValuesGermany, closingValuesGermany, maximumValuesGermany, minimumValuesGermany)

freqBearishEngulfingGermany <- length(which(resultsBearishEngulgingGermany == 1))
freqBullishEngulfingGermany <- length(which(resultsBullishEngulfingGermany == 1))
freqHammerGermany <- length(which(resultsHammerGermany == 1))
freqHangingManGermany <- length(which(resultsHangingManGermany == 1))
# Statistical Significance

indicies <- rep(0, 1)
counter <- 1
for(i in 1:141)
{
  if(resultsBearishEngulgingGermany[[i]] == 1)
  {
    indicies[[counter]] <- i
    counter <- counter + 1
  }
}

bearishEngulfingConsiderGermany <- germanyDataFrameCandlestick[[2]][[85]]

statSigBearishEngulfingGermany <- wilcox.test(bearishEngulfingConsiderGermany, germanyDataFrameCandlestick$closingValuesGermany, mu = 0, alternative = "less")

# Bullish Engulfing Germany
indicies <- rep(0, 4)
counter <- 1
for(i in 1:141)
{
  if(resultsBullishEngulfingGermany[[i]] == 1)
  {
    indicies[[counter]] <- i
    counter <- counter + 1
  }
}

bullishGermanyConsider <- rep(0, 4)
count <- 1
for(i in 1:4)
{
  bullishGermanyConsider[[count]] <- indicies[[i]]
  count <- count + 1
}

statSigGermanyBullshEngulfing <- wilcox.test(bullishGermanyConsider, germanyDataFrameCandlestick$closingValuesGermany, mu = 0, alternative = "greater")

# Hammer Analysis
indicies <- rep(0, 4)
counter <- 1
for(i in 1:141)
{
  if(resultsHammerGermany[[i]] == 1)
  {
    indicies[[counter]] <- i
    counter <- counter + 1
  }
}

hammerGermanyConsider <- rep(0, 4)
count <- 1
for(i in 1:4)
{
  hammerGermanyConsider[[count]] <- indicies[[i]]
  count <- count + 1
}

statSigHammerGermany <- wilcox.test(hammerGermanyConsider, germanyDataFrameCandlestick$closingValuesGermany, mu = 0, alternative = "greater")

# Hanging Man Analysis

indicies <- rep(0, 4)
counter <- 1
for(i in 1:141)
{
  if(resultsHangingManGermany[[i]] == 1)
  {
    indicies[[counter]] <- i
    counter <- counter + 1
  }
}

hangingManConsiderGermany <- rep(0, 4)
count <- 1
for(i in 1:4)
{
  hangingManConsider[[count]] <- indicies[[i]]
  count <- count + 1
}

statSigHangingManGermany <- wilcox.test(hangingManConsiderGermany, germanyDataFrameCandlestick$closingValuesGermany, mu = 0, alternative = "less")

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

resultsDarkCloudCoverCanada <- darkCloudCoverRecoginiton(openingValuesCanada, closingValuesCanada, maximumValuesCanada, minimumValuesCanada)
resultsBearishEngulgingCanada <- bearishEngulfingRecognition(openingValuesCanada, closingValuesCanada, maximumValuesCanada, minimumValuesCanada)
resultsBullishEngulfingCanada <- bullishEnglufingRecognition(openingValuesCanada, closingValuesCanada, maximumValuesCanada, minimumValuesCanada)
resultsHammerCanada <- hammerRecognition(openingValuesCanada, closingValuesCanada, maximumValuesCanada, minimumValuesCanada)
resultsHangingManCanada <- hangingManRecogniton(openingValuesCanada, closingValuesCanada, maximumValuesCanada, minimumValuesCanada)

freqBearishEngulfingCanada <- length(which(resultsBearishEngulgingCanada == 1))
freqBullishEngulfingCanada <- length(which(resultsBullishEngulfingCanada == 1))
freqHammerCanada <- length(which(resultsHammerCanada == 1))
freqHangingManCanada <- length(which(resultsHangingManCanada == 1))

# Statistical Signficance
indicies <- rep(0, 4)
counter <- 1
for(i in 1:141)
{
  if(resultsBearishEngulgingCanada[[i]] == 1)
  {
    indicies[[counter]] <- i
    counter <- counter + 1
  }
}

bearishEngulfingCanadaConsider <- rep(0, 4)
count <- 1
for(i in 1:4)
{
  bearishEngulfingCanadaConsider[[count]] <- indicies[[i]]
  count <- count + 1
}

statSigCanadaBearishEngulfing <- wilcox.test(bearishEngulfingCanadaConsider, canadaCandleStickDataFrame$closingValuesCanada, mu = 0, alternative = "less")
# Bullish Engulfing
indicies <- rep(0, 3)
counter <- 1
for(i in 1:141)
{
  if(resultsBullishEngulfingCanada[[i]] == 1)
  {
    indicies[[counter]] <- i
    counter <- counter + 1
  }
}

bullishEngulfingConsiderCanada <- rep(0, 3)
count <- 1
for(i in 1:3)
{
  bullishEngulfingConsiderCanada[[count]] <- indicies[[i]]
  count <- count + 1
}

statSigCanadaBullishngulfing <- wilcox.test(bullishEngulfingConsiderCanada, canadaCandleStickDataFrame$closingValuesCanada, mu = 0, alternative = "greater")

# Hammer
indicies <- rep(0, 2)
counter <- 1
for(i in 1:141)
{
  if(resultsHammerCanada[[i]] == 1)
  {
    indicies[[counter]] <- i
    counter <- counter + 1
  }
}

hammerConsiderCanada <- rep(0, 2)
count <- 1
for(i in 1:2)
{
  hammerConsiderCanada[[count]] <- indicies[[i]]
  count <- count + 1
}

statSigCanadaHammer <- wilcox.test(hammerConsiderCanada, canadaCandleStickDataFrame$closingValuesCanada, mu = 0, alternative = "greater")

# Hanging Man
indicies <- rep(0, 2)
counter <- 1
for(i in 1:141)
{
  if(resultsHangingManCanada[[i]] == 1)
  {
    indicies[[counter]] <- i
    counter <- counter + 1
  }
}

hangingManConsiderCanada <- rep(0, 2)
count <- 1
for(i in 1:2)
{
  hangingManConsiderCanada[[count]] <- indicies[[i]]
  count <- count + 1
}

statSigCanadaHangingMan <- wilcox.test(hangingManConsiderCanada, canadaCandleStickDataFrame$closingValuesCanada, mu = 0, alternative = "less")
