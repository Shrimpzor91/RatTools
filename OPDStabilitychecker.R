
library(rstatix)
library(writexl)
library(dplyr)
library(readxl)
library(RatTools)
TestData <- data.frame(read_excel("Y:/Victor/OPD 1-24/OPD1-24TestData.xlsx"))

Stabdata <- data.frame(read_excel("Y:/Victor/OPD 1-24/OPD1-24TrainData.xlsx"))
ChoiceColumns <- c(Stabdata$X0sec, Stabdata$X15sec, Stabdata$X30sec, Stabdata$X45sec) 
Stabber <- data.frame(Stabdata$Subject,Stabdata$Day,Stabdata$Group, Stabdata$X0sec, Stabdata$X15sec, Stabdata$X30sec, Stabdata$X45sec) %>%
  setNames (c("Subject", "Day", "Group", "B1","B2","B3","B4"))
TestDays <- sort(unique(TestData$Day))
#Procedure Generated List of all consecutive 3 day training day intervals


result <- lapply(1:(length(TrainingDays) - 2), function(i) create_vectors(TrainingDays, i))
StabilityCheckDays <- setNames(result, TrainingDays[1:(length(TrainingDays) - 2)])

#Manually generated list of appropriate training days

my_list <- list(
  c(14, 15, 16),
  c(15, 16, 17),
  c(16, 17, 18),
  c(17, 18, 21),
  c(18, 21, 24),
  c(21, 24, 27),
  c(24, 27, 28),
  c(27, 28, 29),
  c(28, 29, 32),
  c(35, 29, 32),
  c(38, 35, 32),
  c(50, 52, 55),
  c(58,55,52))



StabberLong <- Stabber %>%
  gather(key = "Block", value = "HR", B1, B2, B3, B4) 

StabberCheck <-lapply(my_list, function(day) {subsetData <- StabberLong %>% filter(Day %in% day) })


ANovaTableFUll <-lapply(my_list, function(day) {
  subsetData <- StabberLong %>% filter(Day %in% day)
  anova_test(data = subsetData, dv = HR, wid = Subject, within = c(Day, Block)) %>%
    get_anova_table()
})
ANovaTableFUll2 <- do.call(rbind,c(ANovaTableFUll))

write_xlsx(ANovaTableFUll2 , "Y:/Victor/StabilityCheck1.xlsx")

#Subsets the gathered data by Group
StabberGroupA <- StabberLong %>% filter(Group == "A")
StabberGroupB <- StabberLong %>% filter(Group == "B")

#Runs Anova tests from subsets of data based on the list of 3 day intervals for each group and saves the tables in lists

anovaTablesA <-lapply(my_list, function(day) {
  subsetData <- StabberGroupA %>% filter(Day %in% day) %>% unique()
  anova_test(data = subsetData, dv = HR, wid = Subject, within = c(Day, Block)) %>%
    get_anova_table()
})
anovaTablesB <-lapply(my_list, function(day) {
  subsetData <- StabberGroupB %>% filter(Day %in% day)
  anova_test(data = subsetData, dv = HR, wid = Subject, within = c(Day, Block)) %>%
    get_anova_table()
})

TrainingDays<-sort(unique(StabberLong$Day))
create_vectors <- function(numbers, start_index) {
  vec <- numbers[start_index:(start_index + 2)]
  return(vec)
}


List2 <- LastXDays_fromList(TestDays,TrainingDays,3)


# Binds the Anova tables by row
anovaTablesA2 <- do.call(rbind,c(anovaTablesA))
anovaTablesB2 <- do.call(rbind,c(anovaTablesB))
# Binds the full Anova tables for each group by column so they can be exported side by side.
FullAnovaTable <-cbind( anovaTablesA2, anovaTablesB2)

write_xlsx(FullAnovaTable , "Y:/Victor/StabilityCheck2.xlsx")


