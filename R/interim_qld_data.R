library(tidyverse);library(readr);library(readxl); library(lubridate)


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
  select(CollectionDate,daily_count) %>% 
  ungroup()

rats_by_date <- rats %>% 
  group_by(DateOfTestResult) %>% 
  mutate(daily_count = sum(n)) %>% 
  filter(AgeAtSubmission == 0, DateOfTestResult >= "2022-01-06") %>% 
  select(DateOfTestResult,daily_count) %>% 
  ungroup()

sum(nocs_by_date$daily_count) + sum(rats_by_date$daily_count)
sum(rats_by_date$daily_count)



linelist_commonwealth <- read_xlsx("PCR and RAT Breakdown (24 hour totals).xlsx",
                                   range = "B4:AC221",sheet = 2,
                                   col_types = c("date",rep("numeric",27))) %>% 
  select(-starts_with("Total"))

states <- names(read_xlsx("PCR and RAT Breakdown (24 hour totals).xlsx",
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
  filter(!is.na(Date))

linelist_commonwealth_QLD <- linelist_commonwealth %>% 
  select(Date,PCR_QLD,RAT_QLD)

combined <- linelist_commonwealth_QLD %>% 
  left_join(nocs_by_date,by = c("Date" = "CollectionDate")) %>% 
  rename("nocs_count" = "daily_count") %>% 
  left_join(rats_by_date,by = c("Date" = "DateOfTestResult")) %>% 
  rename("rats_count" = "daily_count")

combined <- combined %>% rename("comms_PCR" = "PCR_QLD",
                                "comms_RAT" = "RAT_QLD",
                                "state_nocs" = "nocs_count",
                                "state_rats" = "rats_count") %>% 
  pivot_longer(-Date, names_to = "source",
               values_to = "count")

combined %>% ggplot(aes(x = Date,y = count, col = source)) + geom_line()

write_csv(combined,"~/not_synced/qld/combined_state_commons_count_qld.csv")
