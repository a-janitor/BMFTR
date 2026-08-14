##### LOAD PREVIOUS ANALYSIS #####

source(
  here::here(
    "R",
    "04_crossvalidate_reference_models.R"
  )
)

library(glmnet)

set.seed(20260814)

##### NESTED CROSS-VALIDATION FOR ELASTIC NET #####

crossvalidate_elastic_net <- function(
    data,
    outcome,
    predictors,
    outer_folds = 10,
    inner_folds = 10,
    repeats = 10,
    alpha_grid = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    ),
    seed = 20260814
) {
  
  # Nur Personen mit vorhandenem Outcome.
  analysis_data <- data |>
    dplyr::filter(
      !is.na(.data[[outcome]])
    )
  
  all_predictions <- list()
  all_tuning_results <- list()
  
  result_counter <- 1
  
  for (repetition_number in seq_len(repeats)) {
    
    set.seed(seed + repetition_number)
    
    # Äußere Folds für die unabhängige Modellbewertung
    outer_fold_id <- sample(
      rep(
        seq_len(outer_folds),
        length.out = nrow(analysis_data)
      )
    )
    
    for (outer_fold_number in seq_len(outer_folds)) {
      
      message(
        outcome,
        ": repetition ",
        repetition_number,
        "/",
        repeats,
        ", outer fold ",
        outer_fold_number,
        "/",
        outer_folds
      )
      
      outer_training <- analysis_data[
        outer_fold_id != outer_fold_number,
        ,
        drop = FALSE
      ]
      
      outer_testing <- analysis_data[
        outer_fold_id == outer_fold_number,
        ,
        drop = FALSE
      ]
      
      
      ##### IMPUTATION #####
      
      # Imputationswerte werden ausschließlich im äußeren
      # Trainingsdatensatz bestimmt.
      imputed_data <- impute_fold(
        training_data = outer_training,
        testing_data = outer_testing,
        predictors = predictors
      )
      
      outer_training <- imputed_data$training
      outer_testing <- imputed_data$testing
      
      
      ##### MODEL MATRICES #####
      
      matrices <- create_model_matrices(
        training_data = outer_training,
        testing_data = outer_testing,
        predictors = predictors
      )
      
      x_training <- matrices$training
      x_testing <- matrices$testing
      y_training <- outer_training[[outcome]]
      y_testing <- outer_testing[[outcome]]
      
      
      ##### INNER FOLDS #####
      
      # Dieselbe innere Aufteilung wird für alle Alpha-Werte
      # verwendet, damit sie fair verglichen werden.
      set.seed(
        seed +
          repetition_number * 100 +
          outer_fold_number
      )
      
      inner_fold_id <- sample(
        rep(
          seq_len(inner_folds),
          length.out = nrow(outer_training)
        )
      )
      
      
      ##### TUNE ALPHA AND LAMBDA #####
      
      alpha_results <- purrr::map(
        alpha_grid,
        \(current_alpha) {
          
          fitted_cv <- glmnet::cv.glmnet(
            x = x_training,
            y = y_training,
            family = "gaussian",
            alpha = current_alpha,
            foldid = inner_fold_id,
            standardize = TRUE,
            type.measure = "mse"
          )
          
          tibble::tibble(
            alpha = current_alpha,
            lambda = fitted_cv$lambda.min,
            inner_mse = min(fitted_cv$cvm),
            fitted_cv = list(fitted_cv)
          )
        }
      ) |>
        dplyr::bind_rows()
      
      best_alpha_result <- alpha_results |>
        dplyr::slice_min(
          order_by = inner_mse,
          n = 1,
          with_ties = FALSE
        )
      
      best_alpha <- best_alpha_result$alpha
      best_lambda <- best_alpha_result$lambda
      best_cv_model <- best_alpha_result$fitted_cv[[1]]
      
      
      ##### PREDICT OUTER TEST FOLD #####
      
      predicted_values <- stats::predict(
        best_cv_model,
        newx = x_testing,
        s = best_lambda
      ) |>
        as.numeric()
      
      
      ##### SAVE PREDICTIONS #####
      
      all_predictions[[result_counter]] <- tibble::tibble(
        repetition = repetition_number,
        outer_fold = outer_fold_number,
        observed = y_testing,
        predicted = predicted_values
      )
      
      
      ##### SAVE TUNING RESULT #####
      
      coefficients <- stats::coef(
        best_cv_model,
        s = best_lambda
      )
      
      number_nonzero <- sum(
        as.numeric(coefficients) != 0
      ) - 1
      
      all_tuning_results[[result_counter]] <- tibble::tibble(
        repetition = repetition_number,
        outer_fold = outer_fold_number,
        alpha = best_alpha,
        lambda = best_lambda,
        inner_mse = best_alpha_result$inner_mse,
        n_nonzero = number_nonzero
      )
      
      result_counter <- result_counter + 1
    }
  }
  
  
  ##### COMBINE RESULTS #####
  
  predictions <- dplyr::bind_rows(
    all_predictions
  )
  
  tuning_results <- dplyr::bind_rows(
    all_tuning_results
  )
  
  
  ##### PERFORMANCE PER REPETITION #####
  
  performance_by_repetition <- predictions |>
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
    tuning_results = tuning_results,
    performance_by_repetition =
      performance_by_repetition
  )
}

##### INTERNALIZING ELASTIC NET #####

elastic_net_internal <- crossvalidate_elastic_net(
  data = amis_ml,
  outcome = "sdq_internal_t5",
  predictors =
    model_scenarios_internal$M2_maltreatment
)


##### EXTERNALIZING ELASTIC NET #####

elastic_net_external <- crossvalidate_elastic_net(
  data = amis_ml,
  outcome = "sdq_external_t5",
  predictors =
    model_scenarios_external$M2_maltreatment
)

elastic_net_total <- crossvalidate_elastic_net(
  data = amis_ml,
  outcome = "sdq_total_t5",
  predictors =
    model_scenarios_total$M2_maltreatment
)

##### COMBINE ELASTIC-NET PERFORMANCE #####

elastic_net_performance <- dplyr::bind_rows(
  
  elastic_net_internal$performance_by_repetition |>
    dplyr::mutate(
      outcome = "sdq_internal_t5",
      .before = 1
    ),
  
  elastic_net_external$performance_by_repetition |>
    dplyr::mutate(
      outcome = "sdq_external_t5",
      .before = 1
    ),
  
  elastic_net_total$performance_by_repetition |>
    dplyr::mutate(
      outcome = "sdq_total_t5",
      .before = 1
    )
)


##### SUMMARIZE ELASTIC-NET PERFORMANCE #####

elastic_net_performance_summary <-
  elastic_net_performance |>
  dplyr::group_by(
    outcome
  ) |>
  dplyr::summarise(
    n_repeats = dplyr::n(),
    
    rmse_mean = mean(
      rmse,
      na.rm = TRUE
    ),
    
    rmse_sd = stats::sd(
      rmse,
      na.rm = TRUE
    ),
    
    mae_mean = mean(
      mae,
      na.rm = TRUE
    ),
    
    mae_sd = stats::sd(
      mae,
      na.rm = TRUE
    ),
    
    r_squared_mean = mean(
      r_squared,
      na.rm = TRUE
    ),
    
    r_squared_sd = stats::sd(
      r_squared,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


##### PRINT ELASTIC-NET PERFORMANCE #####

print(
  elastic_net_performance_summary,
  n = Inf,
  width = Inf
)

##### SUMMARIZE TUNING RESULTS #####

elastic_net_tuning_summary <- dplyr::bind_rows(
  
  elastic_net_internal$tuning_results |>
    dplyr::mutate(
      outcome = "internal"
    ),
  
  elastic_net_external$tuning_results |>
    dplyr::mutate(
      outcome = "external"
    ),
  
  elastic_net_total$tuning_results |>
    dplyr::mutate(
      outcome = "total"
    )
) |>
  dplyr::group_by(
    outcome,
    alpha
  ) |>
  dplyr::summarise(
    selected_n = dplyr::n(),
    .groups = "drop"
  )


print(
  elastic_net_performance_summary,
  n = Inf
)

##### SAVE CLINICAL MODEL RESULTS #####

clinical_model_results <- list(
  
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
)

saveRDS(
  clinical_model_results,
  file.path(
    path_output,
    "clinical_models_results.rds"
  )
)

message(
  "Clinical model results saved to: ",
  here::here(
    "output",
    "clinical_models_results.rds"
  )
)
