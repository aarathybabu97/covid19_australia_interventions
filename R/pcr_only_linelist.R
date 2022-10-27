
readRDS("outputs/linelist_vic_qld_tas")->linelist_VIC_QLD_TAS

act_commonwealth <- read_csv("~/not_synced/act/act_commonwealth.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y%m%d")) %>%
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
  )

act_commonwealth <- act_commonwealth %>%
  uncount(weights = daily_notification)

act_commonwealth$import_status <- "local"

get_latest_linelist()
#regular_ll <-linelist
regular_ll <- readRDS(paste0("outputs/", get_latest_linelist()))

#sanity check against dubious dates
regular_ll <- regular_ll %>% filter(date_confirmation >= "2020-01-01")

#regular_ll <- readRDS("outputs/testlinelist.RDS")
max(regular_ll$date_confirmation)

# to remove low case counts for the nindss release dates
# regular_ll%>%
#   filter(date_confirmation!=max(regular_ll$date_confirmation))->regular_ll

#use old method for now
old_delay_cdf <- get_notification_delay_cdf(regular_ll)

saveRDS(old_delay_cdf, "outputs/old_method_delay_cdf.RDS")

# for local cases run PCR+RATS data for nsw + VIC. For reff run PCR only
linelist_new <- linelist_VIC_QLD_TAS %>%
  filter(date_confirmation>="2022-01-06")%>% # qld & Vic state summary from 2022-01-06
  bind_rows(act_commonwealth)%>% #act commonwealth data 2022-01-06 to 2022-03-28
  bind_rows(regular_ll%>%
              filter(!state %in% c("QLD","VIC","ACT","TAS")))%>%  # nindss data from beginning for all other states except these
  bind_rows(regular_ll%>%
              filter(state=="ACT",date_confirmation>"2022-03-28"))%>%
  bind_rows(regular_ll %>%
              filter(date_confirmation < "2022-01-06" & state %in% c("QLD","VIC","ACT","TAS")))%>%# bound with qld, act & vic before 2022-01-06
  filter(!state %in% c("NSW","VIC") |test_type!="RAT")

linelist_new$interstate_import[is.na(linelist_new$interstate_import)] <- FALSE
linelist_new$date_linelist[is.na(linelist_new$date_linelist)] <- regular_ll$date_linelist[1]
linelist_new$date_onset <- as_date(ifelse(linelist_new$date_onset < "2020-01-01", NA,linelist_new$date_onset ))

# check linelist

linelist_new %>%
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
             scales = "free") +
  geom_vline(xintercept = linelist_new$date_linelist[1]) + ggtitle("linelist")

set.seed(2020-04-29)
linelist <- linelist_new %>%
  impute_linelist(notification_delay_cdf = old_delay_cdf)

saveRDS(linelist, "outputs/commonwealth_ll_imputed_old_method.RDS")
#linelist <- readRDS("outputs/commonwealth_ll_imputed_old_method.RDS")
