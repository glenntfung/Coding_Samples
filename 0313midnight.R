library(haven)
library(tidyverse)

rm(list = ls())

setwd("C:/Users/fengz/Desktop")

wind <- read_dta('append/windinfo.dta')
colnames(wind)[colnames(wind) == "industry"] <- "Industry"
wind$Industry <- as.character(wind$Industry)
wind$Symbol <- as.factor(wind$Symbol)
wind <- data.frame(wind)

statement <- read_dta('tr/statement_time.dta')
statement <- subset(statement, select = c(Symbol, AccouPeri, ActRelDate))
colnames(statement)[colnames(statement) == "AccouPeri"] <- "AccountPeriod"
colnames(statement)[colnames(statement) == "ActRelDate"] <- "ActualReportDate"
statement$ActualReportDate <- as.Date(statement$ActualReportDate)
statement$AccountPeriod <- as.Date(statement$AccountPeriod)
statement$Symbol <- as.character(statement$Symbol)
statement <- data.frame(statement)

calender <- read_dta('append/calendar.dta')
calender <- subset(calender, select = c(Clddt, State))
colnames(calender)[colnames(calender) == "Clddt"] <- "TradingDate"
calender$TradingDate <- as.Date(calender$TradingDate)
calender$State <- as.factor(calender$State)
calender <- distinct(calender, TradingDate, .keep_all = TRUE)
calender <- data.frame(calender)

turnover <- read_dta('tr/all_priceturnover_raw.dta')
turnover <- subset(turnover, select = c(TradingDate, Symbol))
turnover <- turnover[!is.na(as.numeric(turnover$Symbol)), ]
turnover$Symbol <- as.factor(turnover$Symbol)
turnover$TradingDate <- as.Date(turnover$TradingDate)
turnover <- turnover[!duplicated(turnover),]
turnover <- data.frame(turnover)


replication.statement <- 
  statement[!(statement$AccountPeriod < as.Date("2000-01-01") | 
                statement$AccountPeriod > as.Date("2020-12-31")), ]


replication.statement <- merge(replication.statement, wind, by = "Symbol")


metricdays <- rep(0, length(replication.statement$ActualReportDate))
for(i in 1:length(replication.statement$ActualReportDate)){
  dif <- replication.statement$ActualReportDate[i] - 
    filter(calender, State == "O")$TradingDate
  dif <- ifelse(dif <= 0, dif, NA)
  ind  <- which.max(dif)
  if (is.na(ind) == FALSE) {
  metricdays[i] <- filter(calender, State == "O")$TradingDate[ind]
  } else {
    metricdays[i] <- NA
  }
}
metricdays <- as.Date(metricdays, origin = "1970-01-01")

replication.statement$Metricdate <- metricdays

# rm(metricdays, dif, i, ind)
# 
replication.statement$AccountPeriod <- as.Date(replication.statement$AccountPeriod)
replication.statement$year <- as.numeric(format(replication.statement$AccountPeriod,'%Y'))
replication.statement$month <- as.numeric(format(replication.statement$AccountPeriod,'%m'))
replication.statement$yeartype <- replication.statement$year*10 + replication.statement$month/3


# Define a function to find the nearest past date for each symbol
find_nearest_past_date <- function(announcement_symbol, announcement_date, data) {
  data_subset <- data %>%
    filter(Symbol == announcement_symbol & TradingDate <= announcement_date) %>%
    arrange(desc(TradingDate))
  if (nrow(data_subset) == 0) {
    return(NA)
  } else {
    max_past_date <- max(data_subset$TradingDate)
    if (announcement_date - max_past_date > 28) {
      return(NA)
    } else {
      return(max_past_date)
    }
  }
}


# Apply the function to each row of the announcement dataset for nearest past date
replication.statement <- replication.statement %>%
  rowwise() %>%
  mutate(nearest_past_date = find_nearest_past_date(Symbol, ActualReportDate, turnover))

replication.statement <- replication.statement %>%
  rowwise() %>%
  mutate(nearest_past_date_b = find_nearest_past_date(Symbol, Metricdate, turnover))

# Output the updated announcement dataset
# print(announcement)


# write dta
write_dta(replication.statement, "C:/Users/fengz/Desktop/0313all.dta")
