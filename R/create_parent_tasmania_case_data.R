# Parent file of tasmania case data


tas_first_file <-
  read_csv("~/not_synced/tas/Data request cases Tas 1 Sept to 11 Oct 2022.csv") %>%
  mutate(Date = as.Date(Date, format = "%d-%b")) %>%
  rename(date_confirmation = Date,
         PCR = "Cases from PCR",
         RAT = "Cases from RAT") %>%
  pivot_longer(PCR:Total, names_to = "test_type", values_to = "daily_notification") %>%
  filter(test_type != "Total") %>%
  mutate(state = "TAS")

tas_second_file <-
  readr::read_csv("~/not_synced/tas/20221019 Commonwealth Weekly Report Tasmania.csv") %>%
  select(
    date_confirmation = "current_as_at" ,
    Total = "total_cases_24hrs" ,
    RAT = "rat_cases_24hrs" ,
    PCR = "pcr_cases_24hrs"
  ) %>%
  pivot_longer(PCR:Total, names_to = "test_type", values_to = "daily_notification") %>%
  filter(test_type != "Total") %>%
  mutate(state = "TAS") %>%
  mutate(date_confirmation = as.Date(date_confirmation, format = "%d/%m/%Y"))

bind_rows(tas_first_file, tas_second_file) -> tasmania_case_data

saveRDS(tasmania_case_data,"~/not_synced/tas/parent_tasmania_case_data.RDS")
saveRDS(tasmania_case_data,
        "~/not_synced/tas/tasmania_case_data.RDS")

