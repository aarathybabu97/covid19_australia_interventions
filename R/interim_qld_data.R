# library(tidyverse)
# library(readr)
# library(readxl)
# library(lubridate)
# 
# get_qld_data_dates <- function(){
#   list.dirs(
#     path = "~/not_synced/qld/",
#     full.names = FALSE,
#     recursive = FALSE
#   ) %>%
#     as.Date
# }
# get_qld_data_dates()
# 
# get_qld_data_dir <- function(
#   date = NULL
# ){
#   # get most recent forecast
#   dir_dates <- get_qld_data_dates()
#   
#   if (is.null(date)) {
#     dir_index <- which.max(dir_dates)
#   } else {
#     dir_index <- which(dir_dates == date)
#     if (length(dir_index) != 1){
#       stop("Either no directory or too many directories match this date")
#     }
#   }
#   
#   dir <- list.dirs(
#     path = "~/not_synced/qld/",
#     full.names = TRUE,
#     recursive = FALSE
#   )[dir_index]
#   
#   return(dir)
#   
# }



nocs <- read_csv("~/not_synced/qld/nocs_freya.csv")
rats <- read_csv("~/not_synced/qld/rats_freya.csv")

nocs_30_7 <- nocs %>%
  filter(CollectionDate == "2022-07-30") %>%
  select(n) %>%
  sum()

rats_30_7 <- rats %>%
  filter(DateOfTestResult == "2022-07-30") %>%
  select(n) %>%
  sum()

nocs_by_date <- nocs %>%
  group_by(CollectionDate) %>%
  mutate(daily_count = sum(n)) %>%
  filter(AgeAtOnset == 0, CollectionDate >= "2022-01-06") %>%
  select(CollectionDate, daily_count) %>%
  ungroup() %>%
  rename(PCR_QLD = daily_count)

rats_by_date <- rats %>%
  group_by(DateOfTestResult) %>%
  mutate(daily_count = sum(n)) %>%
  filter(AgeAtSubmission == 0, DateOfTestResult >= "2022-01-06") %>%
  select(DateOfTestResult, daily_count) %>%
  ungroup() %>%
  rename(RAT_QLD = daily_count)

linelist_commonwealth_QLD <- nocs_by_date %>%
  full_join(rats_by_date, by = c("CollectionDate" = "DateOfTestResult")) %>%
  rename("Date" = "CollectionDate") %>%
  mutate(Date = as.Date(Date, format = "%Y%m%d"))

write_csv(linelist_commonwealth_QLD,
          "~/not_synced/qld/linelist_commonwealth_QLD.csv")
