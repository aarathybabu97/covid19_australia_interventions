# Read commonwealth, VIC, QLD , covidlive data. Use THIS week's linelist & delay

source("R/functions.R")
# add queensland data 
source("R/interim_qld_act_data.R")

#overall linelist read line
linelist <- load_linelist(use_vic = FALSE) #skip Vic since using summary data

# remove dubious SA onset dates
# need to look at this again at some point
linelist$date_onset[(linelist$state == "SA" & linelist$date_onset >= as_date("2022-02-27"))] <- NA


#check min & max dates

min_date <- min(linelist$date_confirmation)
max_date <- max(linelist$date_confirmation)

#remove ddubious SA confirmation dates
linelist <- linelist %>% filter(date_confirmation >= "2020-01-23")


saveRDS(
  linelist,
  sprintf(
    "outputs/linelist_%s.RDS",
    linelist$date_linelist[1] %>%
      format.Date(format = "%Y%m%d")
  )
)

gc()
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
#vic_state_count$Date <- vic_state_count$Date+1

qld_state_count <- read_csv("~/not_synced/qld/linelist_commonwealth_QLD.csv")%>% 
  mutate(Date=as.Date(Date,format = "%Y%m%d"))

linelist_VIC_QLD_preprocess <- full_join(vic_state_count,qld_state_count)

#pivot into linelist format
linelist_VIC_QLD <- linelist_VIC_QLD_preprocess %>%
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

linelist_VIC_QLD <- linelist_VIC_QLD %>%
  filter(date_confirmation>="2022-01-06") # qld & Vic state summary from 2022-01-06
linelist_VIC_QLD %>%
  filter(is.na(daily_notification))

linelist_VIC_QLD <- linelist_VIC_QLD %>% dplyr::mutate(daily_notification = replace_na(daily_notification, 0))%>%
  
  uncount(weights = daily_notification)

act_commonwealth<- read_csv("~/not_synced/qld/act_commonwealth.csv")%>% 
  mutate(Date=as.Date(Date,format = "%Y%m%d"))%>%
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
         "state" = word(state,2,2, sep = fixed("_")))

act_commonwealth<-act_commonwealth%>%
  
  uncount(weights = daily_notification) 


get_latest_linelist()
regular_ll <-linelist
# regular_ll <- readRDS(paste0("outputs/",get_latest_linelist()))

#sanity check against dubious dates
regular_ll <- regular_ll %>% filter(date_confirmation >= "2020-01-01")

#regular_ll <- readRDS("outputs/testlinelist.RDS")

#use old method for now
old_delay_cdf <- get_notification_delay_cdf(regular_ll)

 saveRDS(old_delay_cdf,"outputs/old_method_delay_cdf.RDS")

#make into reff data format
linelist_VIC_QLD$import_status <- "local"
act_commonwealth$import_status <- "local"
# linelist_new <- regular_ll %>%
#   filter(date_confirmation >= "2022-01-06" & !state %in% c("QLD","VIC")) %>%
#   bind_rows(linelist_VIC_QLD)

# for vic use linelist till 2022/01/06 & then replace with summary

# linelist_new <- linelist_VIC_QLD %>%
#   filter(date_confirmation>="2022-01-06")%>% # qld & Vic state summary from 2022-01-06
#   bind_rows(regular_ll%>%
#               filter(!state %in% c("QLD","VIC")))%>%  # nindss data from beginning for all states
#   bind_rows(regular_ll %>%
#           filter(date_confirmation < "2022-01-06" & state %in% c("QLD","VIC"))) # bound with qld & vic before 2022-01-06
# 

linelist_new <- linelist_VIC_QLD %>%
  filter(date_confirmation>="2022-01-06")%>% # qld & Vic state summary from 2022-01-06
  bind_rows(act_commonwealth)%>% #act commonwealth data 2022-01-06 to 2022-03-28
  bind_rows(regular_ll%>%
              filter(!state %in% c("QLD","VIC","ACT")))%>%  # nindss data from beginning for all other states except these
  bind_rows(regular_ll%>%
              filter(state=="ACT",date_confirmation>"2022-03-28"))%>%
  bind_rows(regular_ll %>%
              filter(date_confirmation < "2022-01-06" & state %in% c("QLD","VIC","ACT"))) # bound with qld, act & vic before 2022-01-06


linelist_new$interstate_import[is.na(linelist_new$interstate_import)] <- FALSE

linelist_new$date_linelist[is.na(linelist_new$date_linelist)] <- regular_ll$date_linelist[1]

linelist_new$date_onset <- as_date(ifelse(linelist_new$date_onset < "2020-01-01",NA,linelist_new$date_onset))


linelist_new %>%
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
  geom_vline(xintercept = linelist_new$date_linelist[1])

nt_examine <- regular_ll%>%
  filter(state=="NT")%>%
  filter(date_confirmation>="2022-08-25")%>%
  group_by(state, date_confirmation, test_type) %>%
  summarise(cases = n())

nt_examine%>%
  ggplot() +
  geom_bar(
    aes(
      x = date_confirmation,
      y = cases,
      fill = test_type
    ),
    stat = "identity"
  )
#   geom_vline(xintercept = linelist_new$date_linelist[1])


#write it first before imputation for Dylan
#write_linelist(linelist = linelist) # not needed while UoA forecast on hiatus.

#impute - this takes a very long time
#we can choose not to impute and it will be imputed in reff_model_data function instead
#makes no difference, but can impute first to save progress in case of issues

linelist_new %>%
  filter(date_confirmation >= Sys.Date()%m-% months(2)) %>%
  group_by(state, date_confirmation, test_type) %>%
  summarise(cases = n())%>%
  arrange(desc(date_confirmation))->datacheck

set.seed(2020-04-29)
linelist <- linelist_new %>%
  impute_linelist(notification_delay_cdf = old_delay_cdf)

saveRDS(linelist,"outputs/commonwealth_ll_imputed_old_method.RDS")
#linelist <- readRDS("outputs/commonwealth_ll_imputed.RDS")

data <- reff_model_data(linelist_raw = linelist,
                        notification_delay_cdf = old_delay_cdf)
#data[["valid_mat"]][c(919,920),"QLD"] <- FALSE
saveRDS(data, "outputs/pre_loaded_reff_data_old_imputation.RDS")

source("R/watermelon_plot.R")

write_local_cases(data)

