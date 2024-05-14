library(readxl)
library(tidyverse)
library(rstatix)
library(writexl)
library(dplyr)

WBT <- read_excel("Y:/Victor/OPD1-24.xlsx", sheet = "Rdose")
WBT2 = data.frame(WBT )%>%
  
  mutate(Sex = case_when(Rat %in% c("OPD1","OPD2","OPD3","OPD4","OPD5","OPD6",
                                    "OPD07","OPD08","OPD09","OPD10","OPD11","OPD12") ~ "F",
                         Rat  %in% c("OPD13","OPD14","OPD15","OPD16","OPD17","OPD18",
                                     "OPD19","OPD20","OPD21","OPD22","OPD23","OPD24") ~ "M"))


T1data <-  select(WBT2, T1Dose,T1.weight, Sex) %>% rename(
  Dose = T1Dose,
  Weight = T1.weight)
T1data[, 2] <- as.numeric(T1data[, 2]) %>% signif(digits = 2)

T2data <- select(WBT2, T2Dose,T2.weight,Sex)%>% rename(
  Dose = T2Dose,
  Weight = T2.weight)
T2data[, 2] <- as.numeric(T2data[, 2]) %>% signif(digits = 2)

T3data <- select(WBT2, T3Dose,T3.Weight,Sex)%>% rename(
  Dose = T3Dose,
  Weight = T3.Weight)

T3data[, 2] <- as.numeric(T3data[, 2]) %>% signif(digits = 2)

T4data <- select(WBT2, T4Dose,T4.weight,Sex)%>% rename(
  Dose = T4Dose,
  Weight = T4.weight)

T4data[, 2] <- as.numeric(T4data[, 2]) %>% signif(digits = 2)

T5data <- select(WBT2, T5Dose,T5.weight,Sex)%>% rename(
  Dose = T5Dose,
  Weight = T5.weight)

T5data[, 2] <- as.numeric(T5data[, 2]) %>% signif(digits = 2)


T6data <- select(WBT2, T6Dose,T6.weight,Sex)%>% rename(
  Dose = T6Dose,
  Weight = T6.weight)

T6data[, 2] <- as.numeric(T6data[, 2]) %>% signif(digits = 2)

T7data <- select(WBT2, T7Dose,T7.weight,Sex)%>% rename(
  Dose = T7Dose,
  Weight = T7.weight)

T7data[, 2] <- as.numeric(T7data[, 2]) %>% signif(digits = 2)


T1W<-(T1data$Weight)

T1F <-subset(T1data, subset = Sex == "F")
T1M <-subset(T1data, subset = Sex == "M")

T1FW <- (T1F$Weight)
T1MW <- (T1M$Weight)


T2W<-(T2data$Weight)

T2F <-subset(T2data, subset = Sex == "F")
T2M <-subset(T2data, subset = Sex == "M")
T2FW <- (T2F$Weight)
T2MW <- (T2M$Weight)

T3W<-(T3data$Weight)

T3F <-subset(T3data, subset = Sex == "F")
T3M <-subset(T3data, subset = Sex == "M")

T3FW <- (T3F$Weight)
T3MW <- (T3M$Weight)

T4W<-(T4data$Weight)

T4F <-subset(T4data, subset = Sex == "F")
T4M <-subset(T4data, subset = Sex == "M")

T4FW <- (T4F$Weight)
T4MW <- (T4M$Weight)

T5W<-(T5data$Weight)

T5F <-subset(T5data, subset = Sex == "F")
T5M <-subset(T5data, subset = Sex == "M")

T5FW <- (T5F$Weight)
T5MW <- (T5M$Weight)

T5W<-(T5data$Weight)

T6F <-subset(T6data, subset = Sex == "F")
T6M <-subset(T6data, subset = Sex == "M")

T6FW <- (T6F$Weight)
T6MW <- (T6M$Weight)

T6W<-(T6data$Weight)

T7F <-subset(T7data, subset = Sex == "F")
T7M <-subset(T7data, subset = Sex == "M")

T7FW <- (T7F$Weight)
T7MW <- (T7M$Weight)

T7W<-(T6data$Weight)

T1FWMean <- mean (T1FW)
T2FWMean <- mean (T2FW)
T3FWMean <- mean (T3FW)
T4FWMean <- mean (T4FW)
T5FWMean <- mean (T5FW)
T6FWMean <- mean (T6FW)
T7FWMean <- mean (T7FW)

T1MWMean <- mean (T1MW)
T2MWMean <- mean (T2MW)
T3MWMean <- mean (T3MW)
T4MWMean <- mean (T4MW)
T5MWMean <- mean (T5MW)
T6MWMean <- mean (T6MW)
T7MWMean <- mean (T7MW)

FemaleRatWeightMeans <- data.frame(Test.Day.1 = T1FWMean,
                             Test.Day.2 = T2FWMean,
                             Test.Day.3 = T3FWMean,
                             Test.Day.4 = T4FWMean,
                             Test.Day.5 = T5FWMean,
                             Test.Day.6 = T6FWMean,
                             Test.Day.7 = T7FWMean
                             , row.names = c("Average"))

MaleRatWeightMeans <- data.frame(Test.Day.1 = T1MWMean,
                                 Test.Day.2 = T2MWMean,
                                 Test.Day.3 = T3MWMean,
                                 Test.Day.4 = T4MWMean,
                                 Test.Day.5 = T5MWMean,
                                 Test.Day.6 = T6MWMean,
                                 Test.Day.7 = T7MWMean
                                 , row.names = c("Average"))



FRM<- c(T1FWMean,T2FWMean,T3FWMean,T4FWMean,T5FWMean,T6FWMean,T7FWMean)
MRM<- c(T1MWMean,T2MWMean,T3MWMean,T4MWMean,T5MWMean,T6MWMean,T7MWMean)

RatWeightMeans <- data.frame(Female = FRM,Male = MRM,
                             row.names = c("Test1Average",
                                           "Test2Average",
                                           "Test3Average",
                                           "Test4Average",
                                           "Test5Average",
                                           "Test6Average",
                                           "Test7Average"))

T5FWMean

FRMgrowth_rate = RatWeightMeans %>%
  mutate(Diff_growth = Female - lag(Female),
         Rate_percent = (Diff_growth/Female * 100)) 
MRMgrowth_rate = RatWeightMeans %>%
  mutate(Diff_growth = Male - lag(Male),
         Rate_percent = (Diff_growth/Male * 100)) 
MeanGrowthMale <- mean(MRMgrowth_rate$Diff_growth,na.rm=TRUE)
MeanGrowthMalePercent <- mean(MRMgrowth_rate$Rate_percent,na.rm=TRUE)
MeanGrowthFemale <- mean(FRMgrowth_rate$Diff_growth,na.rm=TRUE)
MeanGrowthFemalePercent <- mean(FRMgrowth_rate$Rate_percent,na.rm=TRUE)

