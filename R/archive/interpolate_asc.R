source("R/functions.R")
library(zoo)

CAR_time_point <- read_csv("outputs/at_least_one_sym_states_central_smoothed.csv")

CAR_time_point <- CAR_time_point %>% 
  select(state,date,fitted_point4) %>% 
  rename("test_prob_given_symptom" = fitted_point4) %>% 
  mutate(test_prob_given_infection = test_prob_given_symptom * 0.75 * 0.01) #scale for percentage

#make smooth df
CAR_smooth <- tibble(date = rep(data$dates$infection,
                                length(data$state)),
                     state = rep(data$states,
                                 each = length(data$dates$infection))
                     )

CAR_smooth <- left_join(CAR_smooth,CAR_time_point)

#maybe no use conditional on symptom column for now
CAR_smooth <- CAR_smooth %>% select(-test_prob_given_symptom)

#hack in 1s for Delta period
CAR_smooth <- CAR_smooth %>% 
  mutate(test_prob_given_infection = 
  case_when(date <= as_date("2021-12-01") ~ 1,
            TRUE ~ test_prob_given_infection))

#hack in constant for the present period
CAR_smooth <- CAR_smooth %>% 
  group_by(state) %>% 
  mutate(test_prob_given_infection = 
           case_when(date >= as_date("2022-09-21") ~ replace_na(test_prob_given_infection,
                                                                test_prob_given_infection[date == as_date("2022-09-21")]),
                     TRUE ~ test_prob_given_infection))

#smooth
CAR_smooth <- CAR_smooth %>% 
  group_by(state) %>% 
  mutate(test_prob_given_infection = zoo::na.approx(test_prob_given_infection))


CAR_smooth %>% filter(date > as_date("2021-11-01")) %>% 
  ggplot(aes(x = date,
             y = test_prob_given_infection)) + 
  geom_line() + 
  facet_wrap(~state,scales = "free")

#get matrix form
CAR_smooth_mat <- CAR_smooth %>% 
  pivot_wider(values_from = test_prob_given_infection,
                                             names_from = state) %>% 
  select(-date) %>% as.matrix()

saveRDS()