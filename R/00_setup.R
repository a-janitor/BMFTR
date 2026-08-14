##### PROJECT SETUP #####

set.seed(20260814)

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE
)


required_packages <- c(
  "dplyr",
  "purrr",
  "stringr",
  "tibble",
  "readxl",
  "janitor",
  "here"
)


##### INSTALL MISSING PACKAGES #####

missing_packages <- required_packages[
  !required_packages %in%
    rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  
  install.packages(
    missing_packages,
    dependencies = TRUE
  )
  
}


##### LOAD PACKAGES #####

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

##### CREATE MODEL MATRIX #####

create_model_matrices <- function(
    training_data,
    testing_data,
    predictors
) {
  
  # Nur Prädiktoren übernehmen
  training_predictors <- training_data |>
    dplyr::select(
      dplyr::all_of(predictors)
    )
  
  testing_predictors <- testing_data |>
    dplyr::select(
      dplyr::all_of(predictors)
    )
  
  # Faktoren werden in Dummyvariablen umgewandelt.
  training_matrix <- stats::model.matrix(
    ~ . - 1,
    data = training_predictors
  )
  
  testing_matrix <- stats::model.matrix(
    ~ . - 1,
    data = testing_predictors
  )
  
  # Sicherheitsprüfung: Training und Test müssen
  # dieselben Modellspalten besitzen.
  if (!identical(
    colnames(training_matrix),
    colnames(testing_matrix)
  )) {
    stop(
      "Training and testing matrices have different columns."
    )
  }
  
  list(
    training = training_matrix,
    testing = testing_matrix
  )
}

##### FUNCTION: RUN GENETIC SCENARIOS #####

run_genetic_scenarios <- function(
  data,
  outcome,
  scenarios
) {

  purrr::imap(
    scenarios,
    \(predictors, model_name) {

      message(
        "\nRunning ",
        model_name,
        " for ",
        outcome,
        "...\n"
      )

      result <- crossvalidate_elastic_net(
        data = data,
        outcome = outcome,
        predictors = predictors,
        outer_folds = 10,
        inner_folds = 10,
        repeats = 10,
        seed = 20260814
      )

      list(
        performance = result$performance_by_repetition |>
          dplyr::mutate(
            outcome = outcome,
            model = model_name,
            .before = 1
          ),

        tuning = result$tuning_results |>
          dplyr::mutate(
            outcome = outcome,
            model = model_name,
            .before = 1
          )
      )
    }
  )
}

##### RUN MULTIPLE ELASTIC-NET SCENARIOS #####

# Diese Funktion erhält:
# - einen Datensatz,
# - ein Outcome,
# - mehrere benannte Prädiktor-Sets.
#
# Für jedes Prädiktor-Set wird die bereits definierte Funktion
# crossvalidate_elastic_net() ausgeführt.

run_elastic_scenarios <- function(
    data,
    outcome,
    scenarios,
    outer_folds = 10,
    inner_folds = 10,
    repeats = 10,
    seed = 20260814
) {
  
  purrr::imap(
    scenarios,
    \(predictors, model_name) {
      
      message(
        "\nRunning ",
        model_name,
        " for ",
        outcome,
        "...\n"
      )
      
      result <- crossvalidate_elastic_net(
        data = data,
        outcome = outcome,
        predictors = predictors,
        outer_folds = outer_folds,
        inner_folds = inner_folds,
        repeats = repeats,
        seed = seed
      )
      
      list(
        
        # Kreuzvalidierte Modellgüte für jede Wiederholung
        performance =
          result$performance_by_repetition |>
          dplyr::mutate(
            outcome = outcome,
            model = model_name,
            .before = 1
          ),
        
        # Ausgewählte Alpha- und Lambda-Werte
        tuning =
          result$tuning_results |>
          dplyr::mutate(
            outcome = outcome,
            model = model_name,
            .before = 1
          ),
        
        # Vorhersagen sämtlicher äußerer Test-Folds
        predictions =
          result$predictions |>
          dplyr::mutate(
            outcome = outcome,
            model = model_name,
            .before = 1
          )
      )
    }
  )
}


##### CHECK PROJECT #####

message("Project directory: ", here::here())
message("Setup completed successfully.")