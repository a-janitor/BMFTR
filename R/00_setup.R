##### PROJECT SETUP #####

rm(list = ls())

set.seed(20260814)

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE
)


##### REQUIRED PACKAGES #####

required_packages <- c(
  "tidyverse",
  "tidymodels",
  "workflowsets",
  "here",
  "janitor",
  "readxl",
  "haven",
  "skimr",
  "glmnet",
  "ranger",
  "xgboost",
  "vip"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)


##### PATHS #####

path_raw <- here::here("data", "raw")
path_processed <- here::here("data", "processed")
path_config <- here::here("config")
path_output <- here::here("output")


##### CHECK PROJECT #####

message("Project directory: ", here::here())
message("Setup completed successfully.")