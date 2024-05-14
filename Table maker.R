library(readxl)
library(tidyverse)
library(writexl)
library(dplyr)
library(RatTools)

Subjectnum <- as.character (c(paste0("0","1":"9",sep = ""), paste0("10": "24")))
Ganum <- as.character(c(c("01","02","03","07"), paste0(seq(7, 23, by = 2))))
Gbnum <- setdiff(Subjectnum, Ganum)
allSubjects <- paste("OPD", Subjectnum, sep = "")
GroupA <- paste("OPD", Ganum, sep = "")
GroupB <- paste("OPD", Gbnum, sep = "")
Female <- paste0("OPD",head(Subjectnum, 12), sep = "")
Male <-  paste0("OPD",tail(Subjectnum, 12), sep = "")
Conditions <- c("Nal V", "Nal L", "Nal M", "Nal H","Ati V", "Ati L", "Ati M", "Ati H")
Nconditions <- c("Nal V", "Nal L", "Nal M", "Nal H")
Aconditions <- c("Ati V", "Ati L", "Ati M", "Ati H")
test_types <- c("Test", "24hr")
TestDayData <- read_excel("Y:/Victor/OPD 1-24/OPD1-24FullTestData.xlsx")
TDD = data.frame(TestDayData )
TDD$Date <- as.Date(TDD$Date)
TestDatesDF <- read_excel("Y:/Victor/OPD 1-24/OPD1-24 Test Schedule.xlsx", sheet = "TestDaySheet") %>%
  mutate(`Test Days` = as.Date(gsub(" UTC", "", `Test Days`)))%>%
  mutate(`24hr Test Days` = as.Date(gsub(" UTC", "", `24hr Test Days`)))%>%
  mutate(`Group A Test Days` = as.Date(gsub(" UTC", "", `Test Days`)))%>%
  mutate(`Group A 24hr Test Days` = as.Date(gsub(" UTC", "", `24hr Test Days`)))%>%
  mutate(`Group B Test Days` = as.Date(gsub(" UTC", "", `Test Days`)))%>%
  mutate(`Group B 24hr Test Days` = as.Date(gsub(" UTC", "", `24hr Test Days`)))
GroupATestDays <- TestDatesDF$`Group A Test Days`
GroupBTestDays <- TestDatesDF$`Group B Test Days`


formatted_test_dates <- as.Date(TestDatesDF$`Test Days`, format = "%B %d, %Y")
formatted_testdates24hr <- as.Date(TestDatesDF$`24hr Test Days`,format = "%B %d, %Y")
TrainingData <- read_excel("Y:/Victor/OPD 1-24/OPD1-24TrainData.xlsx")
TrainingData<- mutate(TrainingData , Test.Type = "Training") %>%
  mutate(TrainingData , TestNumber = "NA")

Trainingsdays <- as.vector(unique(TrainingData$Day))
FTrainingData <- subset(TrainingData, select = c("Subject","Sex", "Group", "Pellets","Loco","Omit","X0sec",
                               "X15sec","X30sec", "X45sec","X0sLAT","X15sLAT","X30sLAT",
                               "X45sLAT","Omit0s","Omit15s","Omit30s","Omit45s","Day","Condition","Date","Test.Type","TestNumber"))

GroupATDD<- TDD %>% filter(Group == "A") %>%
  mutate(Order = dense_rank(Date))
TDD <- TDD %>%
 mutate(Test.Type = case_when(
  Date %in% formatted_test_dates ~ "Test",
  Date %in% formatted_testdates24hr ~ "24hr",
  TRUE ~ NA_character_
))


Studd <- subset(TDD, Test.Type == "Test" & Condition %in% Conditions)
RatDataList <- Subli(Studd, "Subject")

add_order_column <- function(df) {
  df <- mutate(df, Order = dense_rank(Date))
  return(df)
}

RatDataList2 <- lapply(RatDataList, add_order_column)
TDDB <- do.call(rbind, RatDataList2)
TDDB$Order

FTDD <- subset(TDDB, select = c("Subject","Sex", "Order", "Group", "Pellets","Loco","Omit","X0sec",
                             "X15sec","X30sec", "X45sec","X0sLAT","X15sLAT","X30sLAT",
                             "X45sLAT","Omit0s","Omit15s","Omit30s","Omit45s","Day","Condition","Date","Test.Type"))

Studd2 <- subset(TDD, Test.Type == "24hr" & Condition %in% Conditions)
RatDataListz <- Subli(Studd2, "Subject")

RatDataList2z <- lapply(RatDataListz, add_order_column)
TDDB2 <- do.call(rbind, RatDataList2z)
TDDB2$Order

FTDD2 <- subset(TDDB2, select = c("Subject","Sex", "Order", "Group", "Pellets","Loco","Omit","X0sec",
                               "X15sec","X30sec", "X45sec","X0sLAT","X15sLAT","X30sLAT",
                               "X45sLAT","Omit0s","Omit15s","Omit30s","Omit45s","Day","Condition","Date","Test.Type"))



NaloxoneData <- subset(FTDD, Condition %in% Nconditions & Test.Type == "Test")
AticaprantData <- subset(FTDD, Condition %in% c("Ati V","Ati L","Ati M","Ati H") & Test.Type == "Test")
AticaprantData24 <- subset(FTDD2, Condition %in% c("Ati V","Ati L","Ati M","Ati H")& Test.Type == "24hr")

TDD

NalDATA <- list()
AtiDATA <- list()
AtiDATA24 <- list()



for (i in Nconditions) {
  SD <- subset(NaloxoneData, Condition == i)
  SD <- SD[order(SD$Subject), ]
  NalDATA[[i]] <- SD
}

for (i in Aconditions) {
  SD <- subset(AticaprantData, Condition == i)
  AtiDATA[[i]] <- SD
}

for (i in Aconditions) {
  SD <- subset(AticaprantData24, Condition == i)
  AtiDATA24[[i]] <- SD
}

SubjectSort <- function(df) {
  df[order(df$Subject), ]
}

ListFTDDbycondtion <- Subli(FTDD, "Condition")
SubliEnv(FTDD, "Condition")

NalDATA <- lapply(NalDATA, SubjectSort)
AtiDATA <- lapply(AtiDATA, SubjectSort)
AtiDATA24 <- lapply(AtiDATA24, SubjectSort)


AtiTestTable<- do.call(rbind, AtiDATA)
Ati24TestTable<- do.call(rbind, AtiDATA24)
NalTestTable<- do.call(rbind, NalDATA)

BadRats <- subset(TDD, X0sec < 0.6 & Condition %in% c("Nal V","Ati V" ))
Badratdf <- data.frame(BadRats$Subject,BadRats$Day) %>% rename(Subject = BadRats.Subject, Day = BadRats.Day)
BadratDaylist <- as.vector(BadRats$Day)
BRTDList <- LastXDays_fromList(BadratDaylist, TrainingDays, 2)
Badratdf$TrainingDays <- BRTDList


BRsubsetList <- list()
  for(i in seq_along(BRTDList)){
  subestx <- subset(TrainingData, Day %in% i)
  BRsubsetList[[i]] <- subestx}

LA <- subset(AtiTestTable, Day == 59 )

LA2 <- subset(Ati24TestTable, Day == 60 )
LN <- subset(NalTestTable, Day == 60 )

write_xlsx(AtiTestTable,"Y:/Victor//gateway.xlsx")
write_xlsx(Ati24TestTable,"Y:/Victor//gateway2.xlsx")
write_xlsx(NalTestTable,"Y:/Victor//gateway3.xlsx")
