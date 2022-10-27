

source("R/pcr_only_linelist.R")
# prepare data for Reff modelling
linelist <- readRDS("outputs/commonwealth_ll_imputed_old_method.RDS")

linelist %>%
  filter(state=="ACT",date_confirmation >= "2022-03-01") %>%
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
  ) 

#old_delay_cdf <- readRDS("outputs/old_method_delay_cdf.RDS")
data <- reff_model_data(linelist_raw = linelist,
                        notification_delay_cdf = NULL,
                        start_date = as_date("2021-06-01"),
                        immunity_effect_path = "outputs/combined_effect_full.RDS",
                        ascertainment_level_for_immunity = 0.5)

tibble(dates=data$dates$infection[210:400],
       infectiousness=data$local$infectiousness[210:400,"ACT"])->actspike_check
actspike_check%>%
  ggplot(aes(x=dates,y=infectiousness))+
  geom_line()

linelist %>%
  filter(state=="ACT",date_onset >= "2021-12-15",date_onset<"2022-07-05") %>%
  group_by(state, date_onset, test_type) %>%
  summarise(cases = n()) %>%
  ggplot() +
  geom_bar(
    aes(
      x = date_onset,
      y = cases,
      fill = test_type
    ),
    stat = "identity"
  ) + geom_line(actspike_check,mapping=aes(x=dates,y=infectiousness))+
  geom_vline(xintercept = as.Date("2022-03-28"))
