install.packages("Y:/Victor/R files/Rpackage/RatTools_0.1.0.tar.gz", repos = NULL, type = "source")
library(RatTools)

library(dplyr)
library(openxlsx2)
library(tidyr)

############################# Rat Names, Groups and Sex Variable Definitions ###############################

Subjectnum <- as.character (c(paste0("0","1":"9",sep = ""), paste0("10": "16")))
allSubjects <- paste("OPM", Subjectnum, sep = "")
Male <- paste0("OPM",head(Subjectnum, 8), sep = "")
Female <-  paste0("OPM",tail(Subjectnum, 8), sep = "")

NoDelayDataOPM <- data.frame(read_xlsx("Y:/Victor/OPMTrainDatacomplete.xlsx", startRow = 1) %>% t())

column_names <- NoDelayDataOPM[1, ]
NoDelayDataOPM <- NoDelayDataOPM[-1, ]
colnames(NoDelayDataOPM) <- column_names
NoDelayDataOPM <- subset(NoDelayDataOPM, select = -Comment )

OPMTestSchedule <- data.frame(read_xlsx("Y:/Victor/OPM 1-16/OPM-TestSchedule.xlsx", startRow = 1))

column_names2 <- OPMTestSchedule[1, ]







NoDelayDataOPM <- rename(NoDelayDataOPM, Block1 = "0sec", Block2 = "15sec", Block3 = "30sec", Block4 = "45sec", OmitIncFT = "Omit")

SubliEnv(NoDelayDataOPM, "Subject")

NoDelay <- data.frame(NoDelayDataOPM) %>%
  mutate(Day = dense_rank(Start.Date))  %>% rename(Condition = Experiment) %>%
  mutate_if(~any(grepl("^-?\\d+(\\.\\d+)?$", .)), as.numeric)%>%
  mutate(Sex = case_when(Subject %in% Male ~ "Male",
                         Subject %in% Female ~ "Female")) %>%
  mutate(Date = as.Date(Start.Date))

str(NoDelay)
NDbyDate <- Subli(NoDelay, "Start.Date")

MalesbyDay <- lapply(NDbyDate, subset, Sex == "Male")
FemalebyDay <- lapply(NDbyDate, subset, Sex == "Female")



NDP <- (do.call(rbind, NDbyDate))
NDP$Omit45s <- ifelse(NDP$L.49. == 0, NDP$Omit45s - 1, NDP$Omit45s) 
NDP$Block1X <- ifelse(10-NDP$Omit0s == 0, 0, NDP$Block1/(10-NDP$Omit0s))
NDP$Block2X <- ifelse(10-NDP$Omit15s == 0, 0, NDP$Block2/(10-NDP$Omit0s))
NDP$Block3X <- ifelse(10-NDP$Omit30s == 0, 0, NDP$Block3/(10-NDP$Omit0s))
NDP$Block4X <- ifelse(10-NDP$Omit45s == 0, 0, NDP$Block4/(10-NDP$Omit0s))
NDP$BlockTotal <- (NDP$Block1+NDP$Block2+NDP$Block2+NDP$Block4)
NDP$BlockMean <- (NDP$Block1+NDP$Block2+NDP$Block3+NDP$Block4)/4
NDP$True.Omit <- (NDP$Omit0s + NDP$Omit15s + NDP$Omit30s + NDP$Omit45s)

OPMMales <- subset(NDP, Sex == "Male")
OPMFemales <- subset(NDP, Sex == "Female")
NDP$Date
NDP$Date <- NDP$Date %>% as.Date( format = "%B %d, %Y")
OPMTestSchedule$Date <-OPMTestSchedule$Date %>% as.Date( format = "%B %d, %Y")
gathered_data <- pivot_longer(OPMTestSchedule, cols = 4:19, names_to = "Subject", values_to = "Condition")
unique_data <- unique(gathered_data)
zz<-data.frame(unique_data$Subject,unique_data$Condition, unique_data$Date) %>%
  rename(Subject = unique_data.Subject, Condition = unique_data.Condition,Date = unique_data.Date)
NDP$Condition <- zz$Condition[match(paste(NDP$Date, NDP$Subject), paste(zz$Date, zz$Subject))] 
NDP<- unique(NDP)
write_xlsx(NDP, "Y:/Victor/OPMData_Nodelay2.xlsx")
View(zz)
AllNum <- ~all(grepl("^-?\\d+(\\.\\d+)?$", .))
AllInt <- ~all(grepl("^-?\\d+?$", .))
