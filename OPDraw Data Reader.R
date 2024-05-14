install.packages("Y:/Victor/R files/Rpackage/RatTools_0.1.0.tar.gz", repos = NULL, type = "source")
library(RatTools)

library(dplyr)
library(openxlsx2)
library(tidyr)

############################# Rat Names, Groups and Sex Variable Definitions ###############################

Subjectnum <- as.character (c(paste0("0","1":"9",sep = ""), paste0("10": "24")))
Ganum <- as.character(c(c("01","02","03","07"), paste0(seq(7, 23, by = 2))))
Gbnum <- setdiff(Subjectnum, Ganum)
allSubjects <- paste("OPD", Subjectnum, sep = "")
GroupA <- paste("OPD", Ganum, sep = "")
GroupB <- paste("OPD", Gbnum, sep = "")
Female <- paste0("OPD",head(Subjectnum, 12), sep = "")
Male <-  paste0("OPD",tail(Subjectnum, 12), sep = "")
Mistakes <- paste("OPD","0","10":"24", sep = "")
Fixed <- paste("OPD","10":"24", sep = "")
TestPrograms <- c("DelayDL_HOUSELIGHTc","DelayDR_HOUSELIGHTc", "DelayDL_HOUSELIGHTa", "DelayDR_HOUSELIGHTa")



D1 <- data.frame(read_xlsx("Y:/Victor/OPDData.xlsx", startRow = 1) %>% t())
column_names <- D1[1, ]
D1 <- D1[-1, ]
colnames(D1) <- column_names
D1$Subject <- ifelse(D1$Subject %in% Mistakes, 
                     Fixed[match(D1$Subject, Mistakes)], 
                     D1$Subject)
D1$Subject %>% unique() %>% sort()

########################## TEST DATES taken from test schedule and formatted ###################################

TestDatesDF <- read_xlsx("Y:/Victor/OPD 1-24/OPD1-24 Test Schedule.xlsx",sheet = "TestDaySheet")
TestDatesDF <- TestDatesDF[,1:2]


formatted_test_dates <- as.Date(TestDatesDF$`Test Days`, format = "%B %d, %Y")
formatted_testdates24hr <- as.Date(TestDatesDF$`24hr Test Days`,format = "%B %d, %Y")
Alltestdays <- c(formatted_test_dates,formatted_testdates24hr)

#All rat data extracted from raw data and formatted.
#Filtered by the correct programs running (redundant in this case)
#Sex, Group, Day and Condition columns created and filled, box testing runs removed.
#All numbers made numeric
#All % columns divided by 10 to get a ratio of hits/block trials. This decimal value is equivalent to a percent where a value of 1 = 100%

DelayD <- data.frame(D1 %>%
  filter(MSN %in% TestPrograms)) %>%
  mutate(Day = dense_rank(Start.Date))  %>% rename(Condition = Experiment,Date = Start.Date) %>%
  mutate_if(~all(grepl("^-?\\d+(\\.\\d+)?$", .)), as.numeric) %>%
  mutate(Sex = case_when(Subject %in% Male ~ "Male",
                         Subject%in% Female ~ "Female")) %>%
  mutate(Group = case_when(Subject %in% GroupA ~ "A",
                           Subject%in% GroupB ~ "B"))  %>%
  mutate(Date = as.Date(Date)) %>% 
  subset(select = -End.Date) %>%
  filter(!((X0sec + X15sec + X30sec + X45sec + X0sLAT + X15sLAT + X30sLAT + X45sLAT) == 0))
DelayD$Omit45s <- ifelse(DelayD$L.49. == 0, DelayD$Omit45s - 1, DelayD$Omit45s)
DelayD$X0sec <- ifelse(10-DelayD$Omit0s == 0, 0, DelayD$X0sec/(10-DelayD$Omit0s))
DelayD$X15sec <- ifelse(10-DelayD$Omit15s == 0, 0, DelayD$X15sec/(10-DelayD$Omit15s))
DelayD$X30sec <- ifelse(10-DelayD$Omit30s == 0, 0, DelayD$X30sec/(10-DelayD$Omit30s))
DelayD$X45sec <- ifelse(10-DelayD$Omit45s == 0, 0, DelayD$X45sec/(10-DelayD$Omit45s))

#Reads Test Schedule from Excel sheet and uses it to write in conditions on test days

TestSchedule<- data.frame(read_xlsx("Y:/Victor/OPD 1-24/OPD1-24 Test Schedule.xlsx",startRow = 1, sheet = "RTS"))
TestSchedule$Date <-TestSchedule$Date %>% as.Date( format = "%B %d, %Y")
gathered_data <- pivot_longer(TestSchedule, cols = 2:25, names_to = "Subject", values_to = "Condition")
unique_data <- unique(gathered_data)
zz<-data.frame(unique_data$Subject,unique_data$Condition, unique_data$Date) %>%
  rename(Subject = unique_data.Subject, Condition = unique_data.Condition,Date = unique_data.Date)
DelayD$Condition <- zz$Condition[match(paste(DelayD$Date, DelayD$Subject), paste(zz$Date, zz$Subject))] 


TestDD <- DelayD %>% filter(Date %in% formatted_test_dates) %>%
  mutate(TestNumber = dense_rank(Date)) 
TestDD24hr <- DelayD %>% filter(Date %in% formatted_testdates24hr) %>%
  mutate(TestNumber = dense_rank(Date)) 
TrainDD <- DelayD %>% filter (!(Date %in% Alltestdays)) 
FullTestDD <- rbind(TestDD,TestDD24hr)

Conditions <- c("Nal V", "Nal L", "Nal M", "Nal H","Ati V", "Ati L", "Ati M", "Ati H")
Nconditions <- c("Nal V", "Nal L", "Nal M", "Nal H")
Aconditions <- c("Ati V", "Ati L", "Ati M", "Ati H")

Ati24raw <- subset(TestDD24hr, Condition %in% Aconditions)
Atiraw <- subset(TestDD, Condition %in% Aconditions)
Nalraw <- subset(TestDD, Condition %in% Nconditions)
##################### View Data by Day Functions ###################################################################################

SubliEnv(DelayD, "Condition")
Shrimp <- Subli(DelayD, "Condition")

########################### Export Data to Excel ##########################################################################


write_xlsx(TestDD24hr, "Y:/Victor/OPD1-24Test24hrData.xlsx")
write_xlsx(TestDD, "Y:/Victor/OPD1-24TestData.xlsx")
write_xlsx(TrainDD, "Y:/Victor/OPD1-24TrainData.xlsx")
write_xlsx(FullTestDD, "Y:/Victor/OPD1-24FullTestData.xlsx")
write_xlsx(DelayD, "Y:/Victor/OPD1-24FullData.xlsx")
write_xlsx(Ati24raw, "Y:/Victor/OPD1-24A24.xlsx")
write_xlsx(Atiraw, "Y:/Victor/OPD1-24A.xlsx")
write_xlsx(Nalraw, "Y:/Victor/OPD1-24N.xlsx")
