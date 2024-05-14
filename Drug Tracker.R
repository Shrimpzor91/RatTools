library(readxl)
library(tidyverse)
library(rstatix)
library(writexl)
library(dplyr)


# Import Data here

WBT <- read_excel("Y:/Victor/OPD1-24.xlsx", sheet = "Rdose")

#Defining Variables for Conditions/Number of Tests/Drug Concentrations/etc.

Conditions <- c("AL", "AH", "AM", "NL", "NH", "NM")
TestNumber <- 13
TN <- 1:TestNumber
AtiTotal <- TestNumber
NalTotal <- TestNumber
conditionCols <- list()
testCols <- list()
AllTests <- list() 
sum_condition <- setNames(rep(0, length(Conditions)), Conditions)
count_condition <- setNames(rep(0, length(Conditions)), Conditions)
LowConcentration = 0.001
MediumConcentration = 0.003
HighConcentration = 0.01
NeedleTipLoss = 0.07
count_col_names <- paste0(Conditions, "_Count")

#Creates a tibble called DrugTracker which generates Test names using lists and fills in the columns using data from WBT

DrugTracker <- tibble()
for (i in TN) {
  conditionCol <- paste0("T", i, "Dose")
  testCol <- paste0("T", i, "weight")
  testname <- paste0("Test ", i,sep = "")
  conditionCols[[i]] <- conditionCol
  testCols[[i]] <- testCol
  colNames <- c(conditionCols[[i]], testCols[[i]])
  Test <- WBT[, colNames]
  names(Test) <- colNames
  AllTests[[testname]] <- Test
#Sums using the conditions list
  sums <- sapply(Conditions, function(cond) sum(Test[, testCols[[i]]][Test[, conditionCols[[i]]] == cond], na.rm = TRUE))
  counts <- sapply(Conditions, function(cond) sum(Test[, conditionCols[[i]]] == cond, na.rm = TRUE))
  result_values <- c(sums, counts)
  
  resultRow <- tibble(TestNumber = i, !!!setNames(result_values, c(Conditions, count_col_names)))

  DrugTracker <- bind_rows(DrugTracker, resultRow)
}
#Final calculations added to the table
DrugTracker <- DrugTracker %>%
  mutate(
    NLmg = NL * LowConcentration + (NL_Count * (NeedleTipLoss * LowConcentration)),
    NMmg = NM * MediumConcentration + (NM_Count * (NeedleTipLoss * MediumConcentration)),
    NHmg = NH * HighConcentration + (NH_Count * (NeedleTipLoss * HighConcentration)),
    
    ALmg = AL * LowConcentration + (AL_Count * (NeedleTipLoss * LowConcentration)),
    AMmg = AM * MediumConcentration + (AM_Count * (NeedleTipLoss * MediumConcentration)),
    AHmg = AH * HighConcentration + (AH_Count * (NeedleTipLoss * HighConcentration))
  )
DrugTracker$NaloxoneTotal <- DrugTracker$NLmg + DrugTracker$NMmg + + DrugTracker$NHmg
DrugTracker$AticaprantTotal <- DrugTracker$ALmg + DrugTracker$AMmg + + DrugTracker$AHmg

# column sums to add to the bottom of the table
AllTestsTotal <- colSums(DrugTracker)
DrugTrackerTotal <- bind_rows(DrugTracker, AllTestsTotal)


#Writes into an excel sheet
write_xlsx(DrugTrackerTotal, "Y:/Victor/DrugTracker.xlsx")



