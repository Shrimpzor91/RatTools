DayView <- function(data, days) {
  Databyday <- split(data, data$Day)
  Daysofinterest <- Databyday[days]
  DataByDaysofInterest <- do.call(rbind, lapply(Daysofinterest, as.data.frame))
  return(DataByDaysofInterest)
}

DaySave <- function(data, days) {
  Databyday <- split(data, data$Day)
  Daysofinterest <- Databyday[days]
  DataByDaysofInterest <- do.call(rbind, Daysofinterest)
  return(DataByDaysofInterest)
}

SubliEnv <- function(data, col) {
  Subset_List <- unique(data[[as.character(col)]])
  df_list <- list()
  df_names <- paste(deparse(substitute(data)),"By", as.character(Subset_List), sep = "_")
  for (u in Subset_List) {
    SubsetDfz <- subset(data, data[[as.character(col)]] == as.character(u))
    df_list[[u]] <- SubsetDfz
  }
  names(df_list) <- df_names
  return(list2env(df_list, envir = .GlobalEnv))
}

LastXDays_fromList <- function(TestDayList, TrainingDayList, x) {
  outputlist <- list()
  for (n in seq_along(TestDayList)){
    valid_days <- TrainingDayList[TrainingDayList <= TestDayList[n]]
    sorted_days <- rev(sort(valid_days))
    closest_preceding_days <- head(sorted_days, x)
    outputlist[[n]]<- closest_preceding_days
  }
  return(outputlist)
}

  
  
Rats_By <- function(RatData, variable, value) {
  Subset_Rat_Data <- t(subset(RatData, RatData[[as.character(variable)]] == as.character(value)))
             return(Subset_Rat_Data) }

Subli <- function(data, col) {
  Subset_List <- unique(data[[as.character(col)]])
  df_list <- list()
  df_names <- paste(deparse(substitute(data)),"By", as.character(Subset_List), sep = "_")
  for (u in Subset_List) {
    SubsetDfz <- subset(data, data[[as.character(col)]] == as.character(u))
    df_list[[u]] <- SubsetDfz
  }
  names(df_list) <- df_names
  return(df_list)
}

uvec <- function(datacol) {
  uniquecolumnvector <- sort(unique(datacol))
  return (uniquecolumnvector)
  
}

'%in%' <- function(x,y) {
  return( sapply(x, function(i) any(i==y)))
}

subsetbylist <- function(data, var, a_list) {
  subsetlist <- list()
  for (i in seq_along( a_list)) {
    subsetData <- subset (data, var %in% i)
    subsetlist[[i]] <- subsetData
  }
  return(subsetlist)
}

AllNum <- ~all(grepl("^-?\\d+(\\.\\d+)?$", .))
AnyNum <- ~any(grepl("^-?\\d+(\\.\\d+)?$", .))

AllInt <- ~all(grepl("^-?\\d$"))
AnyInt <- ~any(grepl("^-?\\d$"))

add_order_column_using_list <- function(df) {
  df <- mutate(df, Order = dense_rank(Date))
  return(df)
}