# check qld data dates
# get_qld_data_dates()

# nocs <- readr::read_csv(paste0("~/not_synced/qld/nocs_freya",get_qld_data_dates(),".csv"))
#
# rats <-readr::read_csv(paste0("~/not_synced/qld/rats_freya",get_qld_data_dates(),".csv"))

nocs <- read_excel("~/not_synced/qld/Uni Melb data.xlsx", sheet = 1)
rats <- read_excel("~/not_synced/qld/Uni Melb data.xlsx", sheet = 2)


nocs_by_date <- nocs %>%
  mutate(CollectionDate = as.Date(CollectionDate, format =  "%d/%m/%Y")) %>%
  group_by(CollectionDate) %>%
  mutate(daily_count = sum(N)) %>%
  filter(CollectionDate >= "2022-01-06") %>%
  select(CollectionDate, daily_count) %>%
  ungroup()  %>% distinct(CollectionDate, .keep_all = TRUE) %>%
  rename(PCR_QLD = daily_count)

rats_by_date <- rats  %>%
  mutate(DateOfTestResult = as.Date(DateOfTestResult, format =  "%d/%m/%Y")) %>%
  group_by(DateOfTestResult) %>%
  mutate(daily_count = sum(N)) %>%
  filter(DateOfTestResult >= as_date("2022-01-06")) %>%
  select(DateOfTestResult, daily_count) %>%
  ungroup() %>% distinct(DateOfTestResult, .keep_all = TRUE) %>%
  rename(RAT_QLD = daily_count)

linelist_commonwealth_QLD <- nocs_by_date %>%
  full_join(rats_by_date, by = c("CollectionDate" = "DateOfTestResult")) %>%
  rename("Date" = "CollectionDate") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

linelist_commonwealth_QLD %>%
  filter(if_any(c(Date, PCR_QLD, RAT_QLD), ~ is.na(.)))


linelist_commonwealth_QLD %>%
  pivot_longer(-Date,
               names_to = "state",
               values_to = "daily_notification") %>%
  mutate(date_confirmation = as_date(Date)) %>%
  arrange(date_confirmation)  %>%
  select(date_confirmation,
         daily_notification,
         state) %>%
  mutate(
    "test_type" = word(state, 1, 1, sep = fixed("_")),
    "state" = word(state, 2, 2, sep = fixed("_"))
  ) %>%
  dplyr::mutate(daily_notification = replace_na(daily_notification, 0)) %>%
  
  uncount(weights = daily_notification) %>%
  filter(date_confirmation >= Sys.Date() %m-% months(2)) %>%
  group_by(state, date_confirmation, test_type) %>%
  summarise(cases = n()) %>%
  ggplot() +
  geom_bar(aes(x = date_confirmation,
               y = cases,
               fill = test_type),
           stat = "identity") +
  facet_wrap(~ state,
             ncol = 2,
             scales = "free")


write_csv(linelist_commonwealth_QLD,
          "~/not_synced/qld/linelist_commonwealth_QLD.csv")

# ACT Commonwealth Data code
# 
# ll_filepath <-
#   "~/not_synced/PCR and RAT Breakdown (24 hour totals).xlsx"
# 
# linelist_commonwealth <- read_xlsx(
#   ll_filepath,
#   skip = 3,
#   sheet = 2,
#   col_types = c("skip", "date", rep("numeric", 27))
# ) %>%
#   select(-starts_with("Total"))
# 
# states <- names(read_xlsx(ll_filepath,
#                           range = "B3:AC3", sheet = 2))
# 
# states <- states[-grep("...", states, fixed = TRUE)]
# 
# states <- rep(states, each = 2)
# 
# #get test type designation
# colnames(linelist_commonwealth) <-
#   word(colnames(linelist_commonwealth), 1, 1, sep = fixed("..."))
# 
# 
# colnames(linelist_commonwealth)[2:19] <-
#   paste(colnames(linelist_commonwealth)[2:19], states, sep = "_")
# 
# #check colnames
# colnames(linelist_commonwealth)
# 
# #remove "total" row
# linelist_commonwealth <- linelist_commonwealth %>%
#   filter(!is.na(Date)) %>%
#   mutate_if(is.numeric, abs)
# 
# linelist_commonwealth %>%
#   select(Date, PCR_ACT, RAT_ACT) %>%
#   #pivot_longer(cols=PCR:RAT,names_to = "test_type",values_to = "n")%>%
#   dplyr::filter(Date <= as.Date("2022-03-28")) %>%
#   arrange(desc(Date)) -> act_commonwealth
# write_csv(act_commonwealth,
#           "~/not_synced/act/act_commonwealth.csv")
# gc()

# Read Tasmania. Run covidlive_linelist script for scraped tas data
#source("R/covidlive_linelist.R")
readRDS("~/not_synced/tas/scraped_tas_before.RDS") -> scraped_tas_before

max(scraped_tas_before$date_confirmation)

tasmania_case_data <- readRDS("~/not_synced/tas/tasmania_case_data.RDS")

tasmania_case_data <- tasmania_case_data%>%
  distinct()

min(tasmania_case_data$date_confirmation)
max(tasmania_case_data$date_confirmation)

tasmania_case_data%>%
  filter(is.na(daily_notification))

  tasmania_case_data %>%
  uncount(weights = daily_notification)%>%
  filter(date_confirmation >= Sys.Date() %m-% months(2)) %>%
  group_by(date_confirmation, test_type) %>%
  summarise(cases = n()) %>%
  ggplot() +
  geom_bar(aes(x = date_confirmation,
               y = cases,
               fill = test_type),
           stat = "identity")


get_latest_tasmania_data()

new_tas_data <- read_latest_tasmania_data()

bind_rows(tasmania_case_data, new_tas_data) -> updated_tas_case_data

updated_tas_case_data <- updated_tas_case_data%>%
  distinct()

updated_tas_case_data%>%
  ggplot(aes(x=date_confirmation,
             y=daily_notification,
             fill=test_type))+
  geom_col()

tas_data <- bind_rows(scraped_tas_before, updated_tas_case_data) %>%
  filter(date_confirmation >= "2022-01-06")


tas_data %>%
  filter(is.na(daily_notification))

tas_data <- tas_data %>%
  uncount(weights = daily_notification)

tas_data %>%
  filter(date_confirmation >= Sys.Date() %m-% months(2)) %>%
  group_by(date_confirmation, test_type) %>%
  summarise(cases = n()) %>%
  ggplot() +
  geom_bar(aes(x = date_confirmation,
               y = cases,
               fill = test_type),
           stat = "identity")

saveRDS(updated_tas_case_data,
        "~/not_synced/tas/tasmania_case_data.RDS")
saveRDS(updated_tas_case_data,
        paste0("~/not_synced/tas/tasmania_case_data_",linelist$date_linelist[1],".RDS"))

 