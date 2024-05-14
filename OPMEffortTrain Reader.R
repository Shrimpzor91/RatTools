library(RatTools)

library(dplyr)
library(openxlsx2)
library(tidyr)

############################# Rat Names, Groups and Sex Variable Definitions ###############################

Subjectnum <- as.character (c(paste0("0","1":"9",sep = ""), paste0("10": "16")))
allSubjects <- paste("OPM", Subjectnum, sep = "")
GroupA <- paste("OPM", Ganum, sep = "")
GroupB <- paste("OPM", Gbnum, sep = "")
Female <- paste0("OPM",head(Subjectnum, 8), sep = "")
Male <-  paste0("OPM",tail(Subjectnum, 8), sep = "")
EffortPrograms <- c("Effort-Train","effort-train")

EffortTrainDataOPM <- data.frame(read_xlsx("Y:/Victor/OPMData_EffortTrain.xlsx", startRow = 1) %>% t())
column_names <- EffortTrainDataOPM[1, ]
EffortTrainDataOPM <- EffortTrainDataOPM[-1, ]
colnames(EffortTrainDataOPM) <- column_names


EffortTrain <- data.frame(EffortTrainDataOPM) %>%
  mutate(Day = dense_rank(Start.Date)) %>%
  mutate_if(~all(grepl("^-?\\d+(\\.\\d+)?$", .)), as.numeric) %>%
  mutate(Sex = case_when(Subject %in% Male ~ "Male",
                         Subject %in% Female ~ "Female")) %>%
  mutate(Date = as.Date(Start.Date)) %>%
  unique()

ETbyDate <- Subli(EffortTrain, "Start.Date")

ETP <- do.call(rbind, ETbyDate)

write_xlsx(ETP, "Y:/Victor/OPMData_EffortTrain2.xlsx")

AllNum <- ~all(grepl("^-?\\d+(\\.\\d+)?$", .))
AllInt <- ~all(grepl("^-?\\d$"))
