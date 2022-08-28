library(dplyr)
library(conmat)
library(tidyr)


# 
# weeks <- 0:10
# 
# 
# 
# ve_predictions <- read_csv("outputs/ve_waning_predictions_omicron.csv")
# 
# 
# ve_test <- ve_predictions %>%
#   filter(
#     outcome == "acquisition",
#     immunity == "omicron_infection",
#     days <=10 # will need to be changed to weeks
#   ) %>%
#   pull(ve_predict_mean)
# 
# states <- c("non-immune", "wk_since_exposure")
# 
# margin <- c(NA, 0:10)
# 
# immunity_matrix <- matrix(
#   nrow = length(margin),
#   ncol = length(margin),
#   dimnames = list(paste0(margin,"_week"), paste0(margin,"_week")),
#   data = 0
# )
# 
# 
# #pr_infection <- 0.1
# 
# margin_length <- length(margin)
# 
# #ve_infection <- c(0, rep(0.2, margin_length - 1))
# ve_infection <- c(0, ve_test)
# relative_risk <- 1 - ve_infection
# 
# weekly_new_infections <- 100
# population <- 1e5
# relative_pr_infection <- relative_risk / sum(relative_risk)
# 
# pr_infection <- relative_pr_infection * weekly_new_infections / population
# 
# pr_not_infection <- 1 - pr_infection
# 
# sub_diag <- (row(immunity_matrix) - col(immunity_matrix)) == 1
# sub_diag[1, 1] <- TRUE
# sub_diag[2, 1] <- FALSE
# sub_diag[margin_length, margin_length] <- TRUE
# sub_diag_index <- which(sub_diag)
# 
# immunity_matrix[sub_diag_index] <- pr_not_infection[]
# immunity_matrix[2, ] <- pr_infection
# 
# immunity_matrix
# # initial_states < as.vector(immunity_matrix)
# # initial_index <- which(initial_states != 0)
# # initial
# 
# state <- rep(1/ margin_length, margin_length)
# 
# state_2 <- immunity_matrix %*% state

# generate function that takes all the pre-calculated bits and takes
# in initial state and weekly infections
# and returns subsequent state, this function can then be iterated
# over infection series (and subsequent state)

matrix_multiply_pre_calc <- function(
  max_weeks = 10
){
  ve_predictions <- read_csv("outputs/ve_waning_predictions_omicron.csv")
  
  ves <- ve_predictions %>%
    filter(
      outcome == "acquisition",
      immunity == "omicron_infection",
      days %in% seq(0, max_weeks * 7, by = 7) # will need to be changed to weeks
    ) %>%
    pull(ve_predict_mean)
  
  margin <- c(NA, 0:10)
  
  immunity_matrix <- matrix(
    nrow = length(margin),
    ncol = length(margin),
    dimnames = list(paste0(margin,"_week"), paste0(margin,"_week")),
    data = 0
  )
  
  margin_length <- length(margin)
  
  #ve_infection <- c(0, rep(0.2, margin_length - 1))
  ve_infection <- c(0, ves)
  relative_risk <- 1 - ve_infection
  
  relative_pr_infection <- relative_risk / sum(relative_risk)
  
  sub_diag <- (row(immunity_matrix) - col(immunity_matrix)) == 1
  sub_diag[1, 1] <- TRUE
  sub_diag[2, 1] <- FALSE
  sub_diag[margin_length, margin_length] <- TRUE
  sub_diag_index <- which(sub_diag)
  
  return(
    module(
      max_weeks,
      ve_infection,
      sub_diag_index,
      relative_pr_infection,
      immunity_matrix
    )
  )
  
}


immunity_matrix_multiply <- function(
  weeks_to_consider = 10,
  state = rep(1/(weeks_to_consider+2), (weeks_to_consider+2)),
  weekly_new_infections = 100,
  population = 1e5,
  pre_calc_output = matrix_multiply_pre_calc()
){
  
  relative_pr_infection <- pre_calc_output$relative_pr_infection
  immunity_matrix <- pre_calc_output$immunity_matrix
  sub_diag_index <- pre_calc_output$sub_diag_index
  
  pr_infection <- relative_pr_infection * weekly_new_infections / population * state
  
  pr_not_infection <- 1 - pr_infection
  
  immunity_matrix[sub_diag_index] <- pr_not_infection[]
  immunity_matrix[2, ] <- pr_infection
  
  new_state <- immunity_matrix %*% state
  
  return(new_state)
  
}

pre_calc_test <- matrix_multiply_pre_calc()

#load real pop and infection data, use NSW as test
NSW_pop <- state_populations() %>% 
  filter(state == "New South Wales") %>% 
  pull(population)

daily_new_infections <- read_csv("outputs/local_cases_input.csv") %>% 
  filter(state == "NSW", 
         date_onset >= as_date("2021-12-01"),
         date_onset <= (as_date("2021-12-01") + weeks(20)-days(1)))

weekly_new_infections <- daily_new_infections %>% 
  mutate(week = 0:(length(date_onset)-1) %/% 7) %>% 
  group_by(week) %>% 
  summarise(count = sum(count)) %>% 
  pull(count)

#init matrix
this_state <- immunity_matrix_multiply(pre_calc_output = pre_calc_test,
                                       state = c(1,rep(0,11)),
                                       weekly_new_infections = 0,
                                       population = NSW_pop)

for (iter in 1:length(weekly_new_infections)){
  
  this_state <- immunity_matrix_multiply(state = this_state,
                                         weekly_new_infections = weekly_new_infections[iter],
                                         pre_calc_output = pre_calc_test,population = NSW_pop)
  #print(iter)
}
this_state


this_state_sim_data <- immunity_matrix_multiply(pre_calc_output = pre_calc_test)
for (iter in 1:100000){
  
  this_state_sim_data <- immunity_matrix_multiply(state = this_state_sim_data,
                                         weekly_new_infections = rpois(1,10),
                                         pre_calc_output = pre_calc_test)
  #print(iter)
}
this_state_sim_data

states <- c("nonimmune", "infected", "vaccinated", "infected-vaccinated")

transitions <- matrix("", 4, 4, dimnames = list(states, states))
transitions[1, 1] <- "non-infection"
transitions[3, 3] <- "non-infection"

transitions[2, 1] <- "infection"
transitions[2, 2] <- "re-infection/non-infection"
transitions[4, 3] <- "infection"
transitions[4, 4] <- "re-infection/non-infection"

transitions[3, 1] <- "vaccinated"
transitions[3, 1] <- "vaccinated"
transitions[4, 2] <- "vaccinated"

transitions[4, 1] <- "infection and vaccination"
transitions


