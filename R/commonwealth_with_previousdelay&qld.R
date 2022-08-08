source("R/functions.R")

#library(readxl); library(tidyverse); library(lubridate);library(rvest)

#### CHECK RAW EXCEL FILE for correct range and sheet no BEFORE LOADING
# the formatting and sheet numbering changes from time to time so need to manually do sanity check

ll_filepath <- "~/not_synced/PCR and RAT Breakdown (24 hour totals).xlsx"

linelist_commonwealth <- read_xlsx(ll_filepath,
                                   range = "B4:AC214",sheet = 2,
                                   col_types = c("date",rep("numeric",27))) %>% 
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

#read in vic to override the missing vic col
vic.files <- list.files("~/not_synced/vic/",pattern = "count", full.names = TRUE)
vic.dates <- vic.files %>%
  basename() %>%
  substr(1, 8) %>%
  as.Date(format = "%Y%m%d")

latest <- which.max(vic.dates)
vic.files <- vic.files[latest]


vic_state_count <- read_csv(vic.files) %>% rename("PCR_VIC" = "confirmed",
                                                  "RAT_VIC" = "probable", 
                                                  "Date" = "date")

#shift date to be in line with commonwealth
vic_state_count$Date <- vic_state_count$Date+1

linelist_commonwealth <- linelist_commonwealth %>% left_join(vic_state_count,by = "Date")

linelist_commonwealth <- linelist_commonwealth %>% select(-c("PCR_VIC.x","RAT_VIC.x")) %>% 
  rename("PCR_VIC" = "PCR_VIC.y",
         "RAT_VIC" = "RAT_VIC.y")

# add queensland data 
source("R/interim_qld_data.R")

qld_state_count <- read_csv("~/not_synced/qld/combined_state_commons_count_qld.csv")%>% 
  mutate(Date=as.Date(Date,format = "%Y%m%d"))%>%
  filter(str_detect(source,"state"))%>%
  pivot_wider(names_from = source,values_from = count)

linelist_commonwealth<- linelist_commonwealth %>% left_join(qld_state_count ,by = "Date")%>%
  select(-c("PCR_QLD","RAT_QLD")) %>%
  rename("PCR_QLD"="state_nocs",
         "RAT_QLD"="state_rats")


#pivot into linelist format
linelist_commonwealth <- linelist_commonwealth %>%
  select(-ends_with("Australia")) %>% 
  pivot_longer(-Date,
               names_to = "state",
               values_to = "daily_notification") %>% 
  mutate(
    date_confirmation = as_date(Date)
  ) %>%
  arrange(
    date_confirmation
  )  %>%
  select(
    date_confirmation,
    daily_notification,
    state
  ) %>% 
  mutate("test_type" = word(state,1,1, sep = fixed("_")),
         "state" = word(state,2,2, sep = fixed("_"))) #%>% 


#add in covidlive
source("R/covidlive_linelist.R")

scraped$test_type <- "Total"

weekends_to_replace <- linelist_commonwealth %>% 
  filter(date_confirmation >= as_date("2022-05-07")) %>% 
  mutate(wday = wday(date_confirmation)) %>% 
  filter(wday == c(7,1)) %>% #NOTE that the weekend cases are reported on Sunday
  select(date_confirmation) %>% 
  unique %>% 
  unlist %>% 
  as_date()

linelist_commonwealth <- linelist_commonwealth %>% 
  filter(!(date_confirmation %in% weekends_to_replace & !(state %in% c("VIC","QLD")))) %>% 
  rbind(scraped[(scraped$date_confirmation %in% weekends_to_replace & !(scraped$state %in% c("VIC","QLD"))),])

#also replace state holidays
# need to revisit this because of the non holiday 0 in ACT


holiday_dates_to_replace <- holiday_dates()
holiday_dates_to_replace$state <- abbreviate_states(holiday_dates_to_replace$state)


holiday_dates_to_replace <- linelist_commonwealth %>%
  filter(date_confirmation >= as_date("2022-05-07")) %>%
  left_join(holiday_dates_to_replace,by = c("state" = "state","date_confirmation" = "date")) %>%
  filter(daily_notification == 0, !(is.na(name))) %>% #this 0 filter is necessary because ACT did provide on public holiday at least once
  select("state","date_confirmation")

linelist_commonwealth <- linelist_commonwealth %>%
  filter(!(date_confirmation %in% holiday_dates_to_replace$date_confirmation &
             !(state %in% c("VIC","QLD")) &
             state %in% holiday_dates_to_replace$state)) %>%
  rbind(scraped[(scraped$date_confirmation %in% holiday_dates_to_replace$date_confirmation &
                   !(scraped$state %in% c("VIC","QLD")) &
                   scraped$state %in% holiday_dates_to_replace$state),])


linelist_commonwealth <- linelist_commonwealth %>% dplyr::mutate(daily_notification = replace_na(daily_notification, 0))%>%
  
  uncount(weights = daily_notification)






# uncount(weights = daily_notification) 


#add column for onset
linelist_commonwealth$date_onset <- NA


old_delay_cdf <- readRDS("~/covid19_australia_interventions/outputs/old_method_delay_cdf.RDS")
# delay_ecdf_plot <- tibble("days" = -3:14,
#                           "new_delay" = notification_delay_cdf(days, NULL,NULL),
#                           "old_delay_Vic" = old_delay_cdf(days,
#                                                           possible_onset_dates = rep("2022-01-01",length(days)),
#                                                                        "VIC"),
#                           "old_delay_other" = old_delay_cdf(days,
#                                                           possible_onset_dates = rep("2022-01-01",length(days)),
#                                                           "NSW"))
# delay_ecdf_plot <- delay_ecdf_plot %>% pivot_longer(cols = 2:4,names_to = "type")
# 
# delay_ecdf_plot %>% filter(days <= 10) %>% ggplot(aes(x = days,y = value, col = type)) + geom_line() + scale_x_continuous(breaks = -3:10, limits = c(-3,10))
# 
# ggsave("outputs/figures/ecdf_delay_compare.png")


# we only need the NSW and the pre 6th Jan part of the regular linelist
# so use older linelist, and stitch in newer NSW linelsit

# run get_nsw_linelist to get the new NSW data, 
# remove any NSW cases in the old regular linelist and put in the new NSW data

regular_ll <- readRDS("~/covid19_australia_interventions/outputs/linelist_20220726.RDS")

linelist_commonwealth$import_status <- "local"

nsw_ll <- get_nsw_linelist()

linelist <- regular_ll %>%
  filter(
    !(state == "NSW")
  ) %>%
  bind_rows(
    nsw_ll
  ) %>%
  filter(date_confirmation < "2022-01-06"|state == "NSW")

linelist <- linelist_commonwealth %>%
  filter(date_confirmation >= "2022-01-06" & state != "NSW") %>%
  bind_rows(linelist)

linelist$interstate_import[is.na(linelist$interstate_import)] <- FALSE

linelist$date_linelist[is.na(linelist$date_linelist)] <- regular_ll$date_linelist[1]

linelist$date_onset <- as_date(ifelse(linelist$date_onset < "2020-01-01",NA,linelist$date_onset))


 linelist %>%
  filter(date_confirmation >= Sys.Date()%m-% months(2)) %>%
  group_by(state, date_confirmation, test_type) %>%
  summarise(cases = n()) %>%
  ggplot() +
  geom_bar(
    aes(
      x = date_confirmation,
      y = cases,
      fill = test_type
    ),
    stat = "identity"
  ) +
  facet_wrap(
    ~ state,
    ncol = 2,
    scales = "free"
  ) +
  geom_vline(xintercept = linelist$date_linelist[1])



#write it first before imputation for Dylan
#write_linelist(linelist = linelist) # not needed while UoA forecast on hiatus.

#impute - this takes a very long time
#we can choose not to impute and it will be imputed in reff_model_data function instead
#makes no difference, but can impute first to save progress in case of issues
set.seed(2020-04-29)
linelist <- linelist %>%
  impute_linelist(notification_delay_cdf = old_delay_cdf)

saveRDS(linelist,"outputs/commonwealth_ll_imputed_old_method.RDS")
#linelist <- readRDS("outputs/commonwealth_ll_imputed.RDS")

data <- reff_model_data(linelist_raw = linelist,
                        notification_delay_cdf = old_delay_cdf)
#data[["valid_mat"]][c(919,920),"QLD"] <- FALSE
saveRDS(data, "outputs/pre_loaded_reff_data_old_imputation.RDS")

source("R/watermelon_plot.R")

write_local_cases(data)