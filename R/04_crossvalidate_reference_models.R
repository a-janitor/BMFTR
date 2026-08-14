##### LOAD ANALYSIS DATA #####

source(
  here::here(
    "R",
    "03_prepare_analysis_data.R"
  )
)

set.seed(20260814)


##### PREPARE VARIABLE CODING #####

amis_ml <- amis_ml |>
  dplyr::mutate(
    
    # Geschlecht bleibt eine kategoriale Variable.
    sex = factor(sex),
    
    # Bildung wird zunächst als ordinale numerische Variable
    # mit den vorhandenen Stufen 0 bis 4 behandelt.
    maternal_education = as.numeric(
      as.character(maternal_education)
    )
  )


##### FUNCTION: IMPUTE PREDICTORS #####

# Die Imputationswerte werden ausschließlich aus den Trainingsdaten
# bestimmt. Dadurch gelangen keine Informationen aus dem Test-Fold
# in das trainierte Modell.

impute_fold <- function(
    training_data,
    testing_data,
    predictors
) {
  
  for (variable in predictors) {
    
    # Numerische Variablen: Median aus dem Trainingsdatensatz
    if (is.numeric(training_data[[variable]])) {
      
      training_median <- stats::median(
        training_data[[variable]],
        na.rm = TRUE
      )
      
      training_data[[variable]][
        is.na(training_data[[variable]])
      ] <- training_median
      
      testing_data[[variable]][
        is.na(testing_data[[variable]])
      ] <- training_median
    }
    
    # Kategoriale Variablen: häufigste Kategorie
    if (is.factor(training_data[[variable]])) {
      
      observed_values <- training_data[[variable]][
        !is.na(training_data[[variable]])
      ]
      
      training_mode <- names(
        sort(
          table(observed_values),
          decreasing = TRUE
        )
      )[1]
      
      training_data[[variable]][
        is.na(training_data[[variable]])
      ] <- training_mode
      
      testing_data[[variable]][
        is.na(testing_data[[variable]])
      ] <- training_mode
      
      # Gleiche Faktorstufen in Training und Test sicherstellen
      testing_data[[variable]] <- factor(
        testing_data[[variable]],
        levels = levels(training_data[[variable]])
      )
    }
  }
  
  list(
    training = training_data,
    testing = testing_data
  )
}


##### FUNCTION: CALCULATE PERFORMANCE #####

calculate_performance <- function(
    observed,
    predicted
) {
  
  tibble::tibble(
    
    rmse = sqrt(
      mean(
        (observed - predicted)^2
      )
    ),
    
    mae = mean(
      abs(observed - predicted)
    ),
    
    r_squared = 1 -
      sum(
        (observed - predicted)^2
      ) /
      sum(
        (observed - mean(observed))^2
      )
  )
}


##### FUNCTION: REPEATED CROSS-VALIDATION #####

crossvalidate_lm <- function(
    data,
    outcome,
    predictors,
    folds = 10,
    repeats = 10,
    seed = 20260814
) {
  
  # Personen ohne Outcome können für dieses konkrete
  # Vorhersagemodell nicht zur Evaluation verwendet werden.
  analysis_data <- data |>
    dplyr::filter(
      !is.na(.data[[outcome]])
    )
  
  all_results <- vector(
    mode = "list",
    length = repeats
  )
  
  for (repeat_number in seq_len(repeats)) {
    
    set.seed(seed + repeat_number)
    
    # Jede Person wird zufällig einem Fold zugewiesen.
    fold_id <- sample(
      rep(
        seq_len(folds),
        length.out = nrow(analysis_data)
      )
    )
    
    repeat_predictions <- vector(
      mode = "list",
      length = folds
    )
    
    for (fold_number in seq_len(folds)) {
      
      training_data <- analysis_data[
        fold_id != fold_number,
        ,
        drop = FALSE
      ]
      
      testing_data <- analysis_data[
        fold_id == fold_number,
        ,
        drop = FALSE
      ]
      
      # Imputation innerhalb des jeweiligen Folds
      imputed_data <- impute_fold(
        training_data = training_data,
        testing_data = testing_data,
        predictors = predictors
      )
      
      training_data <- imputed_data$training
      testing_data <- imputed_data$testing
      
      # Dynamische Modellformel
      model_formula <- stats::reformulate(
        termlabels = predictors,
        response = outcome
      )
      
      # Referenzmodell schätzen
      fitted_model <- stats::lm(
        formula = model_formula,
        data = training_data
      )
      
      # Vorhersage für Personen im Test-Fold
      predicted_values <- stats::predict(
        fitted_model,
        newdata = testing_data
      )
      
      repeat_predictions[[fold_number]] <- tibble::tibble(
        repetition = repeat_number,
        fold = fold_number,
        observed = testing_data[[outcome]],
        predicted = as.numeric(predicted_values)
      )
    }
    
    all_results[[repeat_number]] <- dplyr::bind_rows(
      repeat_predictions
    )
  }
  
  predictions <- dplyr::bind_rows(
    all_results
  )
  
  # Leistung wird innerhalb jeder Wiederholung berechnet.
  performance_by_repeat <- predictions |>
    dplyr::group_by(repetition) |>
    dplyr::group_modify(
      \(data, key) {
        calculate_performance(
          observed = data$observed,
          predicted = data$predicted
        )
      }
    ) |>
    dplyr::ungroup()
  
  list(
    predictions = predictions,
    performance_by_repeat = performance_by_repeat
  )
}


##### FUNCTION: RUN ALL MODEL SCENARIOS #####

run_model_scenarios <- function(
    data,
    outcome,
    scenarios
) {
  
  purrr::imap(
    scenarios,
    \(predictors, model_name) {
      
      message(
        "Running ",
        model_name,
        " for ",
        outcome,
        "..."
      )
      
      result <- crossvalidate_lm(
        data = data,
        outcome = outcome,
        predictors = predictors,
        folds = 10,
        repeats = 10
      )
      
      result$performance_by_repeat |>
        dplyr::mutate(
          model = model_name,
          outcome = outcome,
          .before = 1
        )
    }
  ) |>
    dplyr::bind_rows()
}


##### RUN INTERNALIZING MODELS #####

performance_internal <- run_model_scenarios(
  data = amis_ml,
  outcome = "sdq_internal_t5",
  scenarios = model_scenarios_internal
)


##### RUN EXTERNALIZING MODELS #####

performance_external <- run_model_scenarios(
  data = amis_ml,
  outcome = "sdq_external_t5",
  scenarios = model_scenarios_external
)

##### RUN TOTAL-DIFFICULTIES MODELS #####

performance_total <- run_model_scenarios(
  data = amis_ml,
  outcome = "sdq_total_t5",
  scenarios = model_scenarios_total
)

##### COMBINE RESULTS #####

reference_performance <- dplyr::bind_rows(
  performance_internal,
  performance_external,
  performance_total
)

##### SUMMARIZE RESULTS #####

reference_performance_summary <- reference_performance |>
  dplyr::group_by(
    outcome,
    model
  ) |>
  dplyr::summarise(
    n_repeats = dplyr::n(),
    
    rmse_mean = mean(rmse),
    rmse_sd = stats::sd(rmse),
    
    mae_mean = mean(mae),
    mae_sd = stats::sd(mae),
    
    r_squared_mean = mean(r_squared),
    r_squared_sd = stats::sd(r_squared),
    
    .groups = "drop"
  ) |>
  dplyr::arrange(
    outcome,
    rmse_mean
  )


##### PRINT RESULTS #####

print(
  reference_performance_summary,
  n = Inf
)


requireNamespace(
  "glmnet",
  quietly = TRUE
)

saveRDS(
  list(
    reference_performance =
      reference_performance,
    
    reference_performance_summary =
      reference_performance_summary,
    
    elastic_net_internal =
      elastic_net_internal,
    
    elastic_net_external =
      elastic_net_external,
    
    elastic_net_total =
      elastic_net_total,
    
    elastic_net_performance =
      elastic_net_performance,
    
    elastic_net_performance_summary =
      elastic_net_performance_summary,
    
    elastic_net_tuning_summary =
      elastic_net_tuning_summary
  ),
  here::here(
    "output",
    "clinical_models_results.rds"
  )
)

file.exists(
  here::here(
    "output",
    "clinical_models_results.rds"
  )
)

