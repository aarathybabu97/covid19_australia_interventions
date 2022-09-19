# check qld data dates 
get_qld_data_dates()

nocs <- readr::read_csv(paste0("~/not_synced/qld/nocs_freya",get_qld_data_dates(),".csv"))
rats <-readr::read_csv(paste0("~/not_synced/qld/rats_freya",get_qld_data_dates(),".csv"))

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
  filter(CollectionDate >= "2022-01-06") %>%
  select(CollectionDate, daily_count) %>%
  ungroup()  %>% distinct(CollectionDate, .keep_all = TRUE)%>%
  rename(PCR_QLD = daily_count)

rats_by_date <- rats %>% 
  group_by(DateOfTestResult) %>% 
  mutate(daily_count = sum(n)) %>% 
  filter(DateOfTestResult >= as_date("2022-01-06")) %>% 
  select(DateOfTestResult,daily_count) %>% 
  ungroup() %>% distinct(DateOfTestResult, .keep_all = TRUE)%>%
  rename(RAT_QLD = daily_count)

linelist_commonwealth_QLD <- nocs_by_date %>%
  full_join(rats_by_date, by = c("CollectionDate" = "DateOfTestResult")) %>%
  rename("Date" = "CollectionDate") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

write_csv(linelist_commonwealth_QLD,
          "~/not_synced/qld/linelist_commonwealth_QLD.csv")
