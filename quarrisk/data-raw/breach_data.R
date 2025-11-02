## code to prepare `breach_data` dataset goes here
library(readr)
library(dplyr)

# read all 4 files
trav_home <- read_csv("data-raw/TUnvacc_HomeQuar_7d_traveller_breach_timeseries.csv")
worker_hot <- read_csv("data-raw/TUnvacc_WUnvacc_HotelQuar_14d_worker_breach_timeseries.csv")
trav_az1 <- read_csv("data-raw/TVaccAZ1_WUnvacc_HotelQuar_14d_traveller_breach_timeseries.csv")
trav_az2 <- read_csv("data-raw/TVaccAZ2_WUnvacc_HotelQuar_14d_traveller_breach_timeseries.csv")

# add scenario labels
trav_home <- trav_home |> mutate(scenario = "Traveller: Unvaccinated, Home 7d")
worker_hot <- worker_hot |> mutate(scenario = "Worker: Unvaccinated, Hotel 14d")
trav_az1 <- trav_az1 |> mutate(scenario = "Traveller: AZ1, Hotel 14d")
trav_az2 <- trav_az2 |> mutate(scenario = "Traveller: AZ2, Hotel 14d")

# combine all
breach_data <- bind_rows(trav_home, worker_hot, trav_az1, trav_az2)

usethis::use_data(breach_data, overwrite = TRUE)


