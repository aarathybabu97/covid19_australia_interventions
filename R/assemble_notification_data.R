
source("R/functions.R")

#load full linelist format data (NINDSS + NCIMS)
get_latest_linelist()

linelist <- readRDS(paste0("outputs/",get_latest_linelist()))

#hack interstate import issue introduced by fast nindss processing
#linelist$interstate_import <- FALSE

plot_linelist_by_confirmation_date(linelist = linelist)

#load all summary format data
summary_data <- get_summary_data()

summary_data %>% filter(date>=(max(summary_data$date)-months(1))) %>% 
  ggplot(aes(x = date, y = cases, fill = test_type)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~state,scales = "free")

#get act for the period where NINDSS had a rat spike issue
act_issue_period <- get_act_summary_data()

#replace linelist components for states with summary data
linelist <- replace_linelist_bits_with_summary(linelist,
                                               summary_data,
                                               states_select = c("VIC","QLD"),
                                               start = as_date("2022-01-06"),
                                               end = NULL)

linelist <- replace_linelist_bits_with_summary(linelist,
                                               act_issue_period,
                                               states_select = c("ACT"),
                                               start = as_date("2022-01-06"),
                                               end = as_date("2022-03-28"))

#check if ACT is properly joined
plot_linelist_by_confirmation_date(linelist = linelist, date_cutoff = "2022-01-01")


#make watermelon style checking plot
plot_linelist_by_confirmation_date(linelist = linelist)
ggsave("outputs/figures/case_count_by_confirmation.png", bg = 'white',height = 5,width = 9)

#use NSW part of the linelist to get delay cdfs for different test modes

#cut off date at the beginning of RAT reporting
delay_to_consider_date_cutoff <- as_date("2022-01-06")

RAT_cdf <- get_notification_delay_cdf(linelist = linelist %>% 
                                        filter(state == "NSW",
                                               date_confirmation >= delay_to_consider_date_cutoff,
                                               test_type == "RAT"))

PCR_cdf <- get_notification_delay_cdf(linelist = linelist %>% 
                                        filter(state == "NSW",
                                               date_confirmation >= delay_to_consider_date_cutoff,
                                               test_type == "PCR"))

#impute onsets separately and then put together, not the most efficient approach
#but works better with legacy code

#check if any case has missing test type
table(linelist$test_type)

set.seed(2020-04-29)
linelist_RAT <- linelist %>% 
  filter(test_type == "RAT") %>% 
  impute_linelist(notification_delay_cdf = RAT_cdf)

linelist_PCR <- linelist %>% 
  filter(test_type == "PCR") %>% 
  impute_linelist(notification_delay_cdf = PCR_cdf)

linelist <- rbind(linelist_RAT,linelist_PCR) %>%
  arrange(state, date_confirmation, date_onset)

rm(linelist_RAT,linelist_PCR)
gc()

saveRDS(linelist,"outputs/imputed_linelist.RDS")
#linelist <- readRDS("outputs/imputed_linelist.RDS")

data <- reff_model_data(linelist_raw = linelist,
                        notification_delay_cdf = NULL,
                        impute_infection_with_CAR = TRUE)
#data[["valid_mat"]][c(919,920),"QLD"] <- FALSE
saveRDS(data, "outputs/pre_loaded_reff_data.RDS")
#data <- readRDS("outputs/pre_loaded_reff_data_old_imputation.RDS")

source("R/watermelon_plot_completion.R")

write_local_cases(data)


#make PCR only version - in dev


data <- reff_model_data(linelist_raw = linelist %>% filter(test_type == "PCR"),
                        notification_delay_cdf = NULL,
                        impute_infection_with_CAR = TRUE)
#data[["valid_mat"]][c(919,920),"QLD"] <- FALSE
saveRDS(data, "outputs/pre_loaded_reff_data_PCR_only.RDS")
#data <- readRDS("outputs/pre_loaded_reff_data_old_imputation.RDS")

source("R/watermelon_plot_completion.R")

 if (!dir.exists("outputs/PCR_only_local_cases")) {
   dir.create("outputs/PCR_only_local_cases")
 }

write_local_cases(data, dir = "outputs/PCR_only_local_cases",suffix = "PCR_only")

