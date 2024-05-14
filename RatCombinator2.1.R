#Welcome to RatCombinator(v2.1)!
install_package(writexl)
install_package(dplyr)
install_package(readxl)
install_package(stringr)
install_package(tidyr)
#Load packages below to get started.

library(readxl)

library(tidyr)

library(stringr)

library(dplyr)

library(writexl)

#This script requires a formatted data set in order to run.
#Please make sure you have a column for each block, and one that labels your days.
#Also make sure you have a column with the rat names.


#Fill in The variables bellow and run the whole script to get perfectly counterbalanced group selection.
#Your results can be viewed afterwards by typing Ratcombinator in console
#This will show you the top 10 counterbalanced rat combinations
#Replace the names below but do not erase quotations.
#If you have sequentially numbered rats use the colon to suggest a range.
#otherwise you will need to type in the numbers in quotes and separate with commas.
#Since this script checks all possible combinations, it will take exponentially longer with additional rats.
#Ratcombinator can handle 16 -20 rats easily but may take longer to do 24. Please to not exceed 24 unless you have a fast computer.
#if you are using less than 4 blocks comment the blocks you aren't using out with #.



ExcelFilePath <-"Y:/Victor/OPD1-24.xlsx"
SheetName <- "Stabber"
Block1name <-"0s%"
Block2name <-"15s%"
Block3name <-"30s%"
Block4name <-"45s%"
RatName <- "OPD"
FemaleRatNumbers <- c("1":"3")
MaleRatNumbers <- c("13":"15")
RatNumbers <- c(FemaleRatNumbers,MaleRatNumbers)
malespergroup <- 3
femalespergroup <- 3
Listofdays <- c(14:16)

YourData <- (read_xlsx(ExcelFilePath, sheet = SheetName))

YourData <- rename(YourData ,Block1 = Block1name,
                   Block2 = Block2name,
                   Block3 = Block3name,
                   Block4 = Block4name)






allrats <- paste(RatName,RatNumbers, sep = "")%>% as.vector()
femalestotal <- paste(RatName,"0",FemaleRatNumbers, sep = "") %>% as.character()
malestotal <- paste(RatName,MaleRatNumbers, sep = "") %>% as.character()





mcombo <- asplit(combn(malestotal, malespergroup),2) 
fcombo <- asplit(combn(femalestotal, femalespergroup),2)

GroupSelectionData <- subset(YourData, Day %in% Listofdays)

if(exists("Block1name" )){
  Block1 <- data.frame(GroupSelectionData$Block1)
Block1$Average <- rowMeans(Block1)
Block1sum <-sum(Block1$Average)}

if(exists("Block2name" )){
  Block2 <- data.frame(GroupSelectionData$Block2)
Block2$Average <- rowMeans(Block2)
Block2sum <-sum(Block2$Average)}

if(exists("Block3name" )){
  Block3 <- data.frame(GroupSelectionData$Block3) 
Block3$Average <- rowMeans(Block3)
Block3sum <-sum(Block3$Average)}

if(exists("Block4name" )){
Block4 <- data.frame(GroupSelectionData$Block4)
Block4$Average <- rowMeans(Block4) 
Block4sum <- sum(Block4$Average)}

if(exists("Block1name" )){BA1 <-Block1$Average %>% as.numeric()} 
if(exists("Block2name" )){BA2 <-Block2$Average %>% as.numeric()}  
if(exists("Block3name" )){BA3 <-Block3$Average %>% as.numeric()} 
if(exists("Block4name" )){BA4 <-Block4$Average %>% as.numeric()}

MeanRatPerformancebyBlock <- data.frame(GroupSelectionData$Rat,(if(exists("Block1name" )){Block1$Average}),
                                          (if(exists("Block2name" )){Block2$Average}),
                                          (if(exists("Block3name" )){Block3$Average}),
                                          (if(exists("Block4name" )){Block4$Average}))

### 3D Arrays for calculation

Males <- subset(MeanRatPerformancebyBlock , MeanRatPerformancebyBlock[,1] %in% malestotal)
Females <- subset(MeanRatPerformancebyBlock , MeanRatPerformancebyBlock[,1] %in% femalestotal)       

if(exists("Block1name" )){Block1males <-  Males[,2] }      
if(exists("Block1name" )){Block1females <- Females[,2]}  
if(exists("Block1name" )){mcombob1 <- asplit(combn(Block1males, malespergroup),2)}  
if(exists("Block1name" )){fcombob1 <- asplit(combn(Block1females, femalespergroup),2)} 
if(exists("Block1name" )){MaleFemaleComboB1 = data.frame(expand.grid(fcombob1,mcombob1)) %>%
  unnest_wider(Var1, names_sep = "") %>%
  unnest_wider(Var2, names_sep = "") }

if(exists("Block2name" )){Block2males <-  Males[,3] %>% as.list() }      
if(exists("Block2name" )){Block2females <- Females[,3] %>% as.list()  }   
if(exists("Block2name" )){mcombob2 <- asplit(combn(Block2males, malespergroup),2)} 
if(exists("Block2name" )){fcombob2 <- asplit(combn(Block2females, femalespergroup),2)}

if(exists("Block2name" )){MaleFemaleComboB2 = data.frame(expand.grid(fcombob2,mcombob2)) %>%
  unnest_wider(Var1, names_sep = "") %>%
  unnest_wider(Var2, names_sep = "") }

if(exists("Block3name" )){Block3males <- Males[,4] %>% as.list()      
Block3females <- Females[,4] %>% as.list()

mcombob3 <- asplit(combn(Block3males, malespergroup),2) 
fcombob3 <- asplit(combn(Block3females, femalespergroup),2)

MaleFemaleComboB3 = data.frame(expand.grid(fcombob3,mcombob3)) %>%
  unnest_wider(Var1, names_sep = "") %>%
  unnest_wider(Var2, names_sep = "") }


if(exists("Block4name" )){
  Block4males <- Males[,5] %>% as.list()      
Block4females <- Females[,5] %>% as.list()  
mcombob4 <- asplit(combn(Block4males, malespergroup),2) 
fcombob4 <- asplit(combn(Block4females, femalespergroup),2)
MaleFemaleComboB4 = data.frame(expand.grid(fcombob4,mcombob4)) %>%
  unnest_wider(Var1, names_sep = "") %>%
  unnest_wider(Var2, names_sep = "") }

if(exists("Block1name" )){TMFCB1 = data.frame(t(MaleFemaleComboB1))}
if(exists("Block2name" )){TMFCB2 = data.frame(t(MaleFemaleComboB2))}
if(exists("Block3name" )){TMFCB3 = data.frame(t(MaleFemaleComboB3))}
if(exists("Block4name" )){TMFCB4 = data.frame(t(MaleFemaleComboB4))}


NumberedGroups <- seq.int(nrow(MaleFemaleComboB1))


if(exists("Block1name" )){TMFCB1[] <- sapply(TMFCB1, as.numeric)}
if(exists("Block2name" )){TMFCB2[]  <- sapply(TMFCB2, as.numeric)}
if(exists("Block3name" )){TMFCB3[] <- sapply(TMFCB3, as.numeric)}
if(exists("Block4name" )){TMFCB4[]  <- sapply(TMFCB4, as.numeric)}


if(exists("Block1name" )){B1A <-colMeans(TMFCB1)}
if(exists("Block2name" )){B2A <-colMeans(TMFCB2)}
if(exists("Block3name" )){B3A <-colMeans(TMFCB3)}
if(exists("Block4name" )){B4A <-colMeans(TMFCB4)}
if(exists("Block1name" )){B1S <-colSums(TMFCB1)}
if(exists("Block2name" )){B2S <-colSums(TMFCB2)}
if(exists("Block3name" )){B3S <-colSums(TMFCB3)}
if(exists("Block4name" )){B4S <-colSums(TMFCB4)}


if(exists("Block1name" )){B1comp <- (Block1sum - B1S) / (length(RatNumbers)/2)}
if(exists("Block2name" )){B2comp <- (Block2sum - B2S) / (length(RatNumbers)/2)}
if(exists("Block3name" )){B3comp <- (Block3sum - B3S) / (length(RatNumbers)/2)}
if(exists("Block4name" )){B4comp <- (Block4sum - B4S) / (length(RatNumbers)/2)}

if(exists("Block1name" )){B1p <-  abs(B1A - B1comp)}
if(exists("Block2name" )){B2p <-  abs(B2A - B2comp)}
if(exists("Block3name" )){B3p <-  abs(B3A - B3comp)} 
if(exists("Block4name" )){B4p <- abs(B4A - B4comp)}
TotalPerformance <- if(exists("Block1name" )){B1p}+if(exists("Block2name" )){B2p}
                                        if(exists("Block3name" )){+B3p} + if(exists("Block4name" )){B4p}


AC <- data.frame(expand.grid(fcombo,mcombo))%>% rename(
  femalerat = Var1,
  malerat = Var2) 
AC$Allrats <- list(allrats)
AC$GroupA <- mapply(c, AC$femalerat,AC$malerat, SIMPLIFY = FALSE)
AC$GroupB <- AC$Allrats[! AC$Allrats %in% AC$GroupA]



Ratcombinator1 <- data.frame (TotalPerformance)

Ratcombinator1$GroupA <- AC$GroupA

Ratcombinator1$GroupB <- AC$GroupB

Ratcombinator = data.frame(Ratcombinator1[1],Ratcombinator1[2],Ratcombinator1[3]) %>%
  rename(Best_To_Worst = TotalPerformance ) %>%
  arrange(Best_To_Worst)%>%
  head(10)
