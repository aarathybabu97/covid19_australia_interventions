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
  mutate(CollectionDate=as.Date(CollectionDate,format =  "%d/%m/%Y"))%>%
  group_by(CollectionDate) %>%
  mutate(daily_count = sum(n)) %>%
  filter(CollectionDate >= "2022-01-06") %>%
  select(CollectionDate, daily_count) %>%
  ungroup()  %>% distinct(CollectionDate, .keep_all = TRUE)%>%
  rename(PCR_QLD = daily_count)

rats_by_date <- rats  %>%
  mutate(DateOfTestResult=as.Date(DateOfTestResult,format =  "%d/%m/%Y"))%>%
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

linelist_commonwealth_QLD%>%
  filter(if_any(c(Date,PCR_QLD,RAT_QLD), ~ is.na(.)))

write_csv(linelist_commonwealth_QLD,
          "~/not_synced/qld/linelist_commonwealth_QLD.csv")



ll_filepath <- "~/not_synced/PCR and RAT Breakdown (24 hour totals).xlsx"

linelist_commonwealth <- read_xlsx(
  ll_filepath,
  skip = 3,
  sheet = 2,
  col_types = c("skip", "date", rep("numeric", 27))
) %>%
  select(-starts_with("Total"))

states <- names(read_xlsx(ll_filepath,
                          range = "B3:AC3",sheet = 2))

states <- states[-grep("...",states,fixed = TRUE)]

states <- rep(states,each = 2)

#get test type designation
colnames(linelist_commonwealth) <- word(colnames(linelist_commonwealth),1,1, sep = fixed("..."))


colnames(linelist_commonwealth)[2:19] <- paste(colnames(linelist_commonwealth)[2:19],states,sep = "_")

#check colnames
colnames(linelist_commonwealth)

#remove "total" row
linelist_commonwealth <- linelist_commonwealth %>%
  filter(!is.na(Date))%>%
  mutate_if(is.numeric,abs)

linelist_commonwealth%>%
  select(Date,PCR_ACT,RAT_ACT)%>%
  #pivot_longer(cols=PCR:RAT,names_to = "test_type",values_to = "n")%>%
  dplyr::filter(Date<=as.Date("2022-03-28"))%>%
  arrange(desc(Date))->act_commonwealth
write_csv(act_commonwealth,
          "~/not_synced/qld/act_commonwealth.csv")
gc()
