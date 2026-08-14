##### LOAD CBCL ANALYSIS #####

source(
  here::here(
    "R",
    "11_add_cbcl.R"
  )
)


##### CHECK REQUIRED ANALYSIS VARIABLES #####

required_analysis_variables <- c(
  "cbcl_internal_t2",
  "cbcl_external_t2",
  "cbcl_total_t2",
  "sdq_internal_parent_t5_z",
  "sdq_external_parent_t5_z",
  "sdq_total_parent_t5_z",
  "sdq_internal_multi_t5",
  "sdq_external_multi_t5",
  "sdq_total_multi_t5",
  "n_internal_informants_t5",
  "n_external_informants_t5",
  "n_total_informants_t5"
)

missing_analysis_variables <- setdiff(
  required_analysis_variables,
  names(amis_ml)
)

if (length(missing_analysis_variables) > 0) {
  stop(
    "The following analysis variables are missing:\n",
    paste(
      missing_analysis_variables,
      collapse = "\n"
    )
  )
}

message(
  "All required CBCL and multi-informant variables are available."
)

##### CREATE MATCHED ANALYSIS SAMPLES #####

amis_ml_multi_internal <- amis_ml |>
  dplyr::filter(
    !is.na(sdq_internal_parent_t5_z),
    !is.na(sdq_internal_multi_t5),
    n_internal_informants_t5 >= 2
  )

amis_ml_multi_external <- amis_ml |>
  dplyr::filter(
    !is.na(sdq_external_parent_t5_z),
    !is.na(sdq_external_multi_t5),
    n_external_informants_t5 >= 2
  )

amis_ml_multi_total <- amis_ml |>
  dplyr::filter(
    !is.na(sdq_total_parent_t5_z),
    !is.na(sdq_total_multi_t5),
    n_total_informants_t5 >= 2
  )


##### PRINT SAMPLE SIZES #####

multi_sample_overview <- tibble::tibble(
  dimension = c(
    "internal",
    "external",
    "total"
  ),
  n = c(
    nrow(amis_ml_multi_internal),
    nrow(amis_ml_multi_external),
    nrow(amis_ml_multi_total)
  )
)

print(
  multi_sample_overview,
  n = Inf
)
##### CHECK CBCL PREDICTOR SCENARIOS #####

print(
  names(cbcl_scenarios_internal)
)

print(
  names(cbcl_scenarios_external)
)

print(
  names(cbcl_scenarios_total)
)

stopifnot(
  "C1_add_cbcl" %in%
    names(cbcl_scenarios_internal),
  
  "C1_add_cbcl" %in%
    names(cbcl_scenarios_external),
  
  "C1_add_cbcl" %in%
    names(cbcl_scenarios_total)
)
##### FUNCTION: COMPARE PARENT AND MULTI-INFORMANT OUTCOMES #####

run_outcome_comparison <- function(
    data,
    dimension,
    parent_outcome,
    multi_outcome,
    predictors,
    seed = 20260814
) {
  
  message(
    "\nRunning parent outcome for ",
    dimension,
    "...\n"
  )
  
  parent_result <- crossvalidate_elastic_net(
    data = data,
    outcome = parent_outcome,
    predictors = predictors,
    outer_folds = 10,
    inner_folds = 10,
    repeats = 10,
    seed = seed
  )
  
  message(
    "\nRunning multi-informant outcome for ",
    dimension,
    "...\n"
  )
  
  multi_result <- crossvalidate_elastic_net(
    data = data,
    outcome = multi_outcome,
    predictors = predictors,
    outer_folds = 10,
    inner_folds = 10,
    repeats = 10,
    seed = seed
  )
  
  list(
    performance = dplyr::bind_rows(
      
      parent_result$performance_by_repetition |>
        dplyr::mutate(
          dimension = dimension,
          outcome_type = "parent",
          .before = 1
        ),
      
      multi_result$performance_by_repetition |>
        dplyr::mutate(
          dimension = dimension,
          outcome_type = "multi_informant",
          .before = 1
        )
    ),
    
    parent_result = parent_result,
    multi_result = multi_result
  )
}
##### RUN INTERNALIZING COMPARISON #####

multi_internal_results <- run_outcome_comparison(
  data = amis_ml_multi_internal,
  dimension = "internal",
  parent_outcome =
    "sdq_internal_parent_t5_z",
  multi_outcome =
    "sdq_internal_multi_t5",
  predictors =
    cbcl_scenarios_internal$C1_add_cbcl
)

##### RUN EXTERNALIZING COMPARISON #####

multi_external_results <- run_outcome_comparison(
  data = amis_ml_multi_external,
  dimension = "external",
  parent_outcome =
    "sdq_external_parent_t5_z",
  multi_outcome =
    "sdq_external_multi_t5",
  predictors =
    cbcl_scenarios_external$C1_add_cbcl
)

##### RUN TOTAL-DIFFICULTIES COMPARISON #####

multi_total_results <- run_outcome_comparison(
  data = amis_ml_multi_total,
  dimension = "total",
  parent_outcome =
    "sdq_total_parent_t5_z",
  multi_outcome =
    "sdq_total_multi_t5",
  predictors =
    cbcl_scenarios_total$C1_add_cbcl
)

##### COMBINE PERFORMANCE #####

multi_outcome_performance <- dplyr::bind_rows(
  multi_internal_results$performance,
  multi_external_results$performance,
  multi_total_results$performance
)


##### SUMMARIZE PERFORMANCE #####

multi_outcome_performance_summary <-
  multi_outcome_performance |>
  dplyr::group_by(
    dimension,
    outcome_type
  ) |>
  dplyr::summarise(
    n_repeats = dplyr::n(),
    
    rmse_mean = mean(rmse),
    rmse_sd = stats::sd(rmse),
    
    mae_mean = mean(mae),
    mae_sd = stats::sd(mae),
    
    r_squared_mean =
      mean(r_squared),
    
    r_squared_sd =
      stats::sd(r_squared),
    
    .groups = "drop"
  )

print(
  multi_outcome_performance_summary,
  n = Inf,
  width = Inf
)
##### CALCULATE PAIRED DIFFERENCES #####

multi_outcome_incremental <- multi_outcome_performance |>
  dplyr::select(
    dimension,
    outcome_type,
    repetition,
    rmse,
    mae,
    r_squared
  ) |>
  tidyr::pivot_wider(
    names_from = outcome_type,
    values_from = c(
      rmse,
      mae,
      r_squared
    )
  ) |>
  dplyr::mutate(
    delta_rmse =
      rmse_multi_informant -
      rmse_parent,
    
    delta_mae =
      mae_multi_informant -
      mae_parent,
    
    delta_r_squared =
      r_squared_multi_informant -
      r_squared_parent
  )


##### SUMMARIZE PAIRED DIFFERENCES #####

multi_outcome_incremental_summary <-
  multi_outcome_incremental |>
  dplyr::group_by(
    dimension
  ) |>
  dplyr::summarise(
    delta_rmse_mean =
      mean(delta_rmse),
    
    delta_rmse_sd =
      stats::sd(delta_rmse),
    
    delta_mae_mean =
      mean(delta_mae),
    
    delta_mae_sd =
      stats::sd(delta_mae),
    
    delta_r_squared_mean =
      mean(delta_r_squared),
    
    delta_r_squared_sd =
      stats::sd(delta_r_squared),
    
    multi_better_rmse_percent =
      mean(delta_rmse < 0) * 100,
    
    multi_better_r_squared_percent =
      mean(delta_r_squared > 0) * 100,
    
    .groups = "drop"
  )

print(
  multi_outcome_incremental_summary,
  n = Inf,
  width = Inf
)
##### CREATE PARENT-CHILD OUTCOMES #####

amis_ml <- amis_ml |>
  dplyr::mutate(
    
    sdq_internal_parent_child_t5 =
      dplyr::if_else(
        !is.na(sdq_internal_parent_t5_z) &
          !is.na(sdq_internal_child_t5_z),
        
        (
          sdq_internal_parent_t5_z +
            sdq_internal_child_t5_z
        ) / 2,
        
        NA_real_
      ),
    
    sdq_external_parent_child_t5 =
      dplyr::if_else(
        !is.na(sdq_external_parent_t5_z) &
          !is.na(sdq_external_child_t5_z),
        
        (
          sdq_external_parent_t5_z +
            sdq_external_child_t5_z
        ) / 2,
        
        NA_real_
      ),
    
    sdq_total_parent_child_t5 =
      dplyr::if_else(
        !is.na(sdq_total_parent_t5_z) &
          !is.na(sdq_total_child_t5_z),
        
        (
          sdq_total_parent_t5_z +
            sdq_total_child_t5_z
        ) / 2,
        
        NA_real_
      )
  )
##### CHECK PARENT-CHILD SAMPLE #####

parent_child_overview <- amis_ml |>
  dplyr::summarise(
    n_internal =
      sum(!is.na(sdq_internal_parent_child_t5)),
    
    n_external =
      sum(!is.na(sdq_external_parent_child_t5)),
    
    n_total =
      sum(!is.na(sdq_total_parent_child_t5))
  )

print(
  parent_child_overview,
  width = Inf
)
##### RUN PARENT-CHILD INTERNALIZING MODELS #####

parent_child_internal_results <-
  run_elastic_scenarios(
    data = amis_ml,
    outcome =
      "sdq_internal_parent_child_t5",
    scenarios =
      cbcl_scenarios_internal
  )


##### RUN PARENT-CHILD EXTERNALIZING MODELS #####

parent_child_external_results <-
  run_elastic_scenarios(
    data = amis_ml,
    outcome =
      "sdq_external_parent_child_t5",
    scenarios =
      cbcl_scenarios_external
  )


##### RUN PARENT-CHILD TOTAL MODELS #####

parent_child_total_results <-
  run_elastic_scenarios(
    data = amis_ml,
    outcome =
      "sdq_total_parent_child_t5",
    scenarios =
      cbcl_scenarios_total
  )


##### EXTRACT PARENT-CHILD PERFORMANCE #####

parent_child_performance <- dplyr::bind_rows(
  
  purrr::map_dfr(
    parent_child_internal_results,
    "performance"
  ),
  
  purrr::map_dfr(
    parent_child_external_results,
    "performance"
  ),
  
  purrr::map_dfr(
    parent_child_total_results,
    "performance"
  )
)
parent_child_performance_summary <-
  parent_child_performance |>
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
    
    r_squared_mean =
      mean(r_squared),
    
    r_squared_sd =
      stats::sd(r_squared),
    
    .groups = "drop"
  )

print(
  parent_child_performance_summary,
  n = Inf,
  width = Inf
)
##### CBCL T2 TO CBCL T5: FULL MODELS #####

cbcl_internal_model <- crossvalidate_elastic_net(
  data = amis_ml,
  outcome = "cbcl_internal_t5",
  predictors = c(
    "sex",
    "age_t2",
    "maternal_education",
    "cbcl_internal_t2",
    "mt_subtypes_t1",
    "mt_frequency_t1",
    "mt_severity_t1"
  )
)

cbcl_external_model <- crossvalidate_elastic_net(
  data = amis_ml,
  outcome = "cbcl_external_t5",
  predictors = c(
    "sex",
    "age_t2",
    "maternal_education",
    "cbcl_external_t2",
    "mt_subtypes_t1",
    "mt_frequency_t1",
    "mt_severity_t1"
  )
)

cbcl_total_model <- crossvalidate_elastic_net(
  data = amis_ml,
  outcome = "cbcl_total_t5",
  predictors = c(
    "sex",
    "age_t2",
    "maternal_education",
    "cbcl_total_t2",
    "mt_subtypes_t1",
    "mt_frequency_t1",
    "mt_severity_t1"
  )
)


##### SUMMARIZE CBCL OUTCOME MODELS #####

cbcl_outcome_performance <- dplyr::bind_rows(
  
  cbcl_internal_model$performance_by_repetition |>
    dplyr::mutate(
      outcome = "cbcl_internal_t5",
      .before = 1
    ),
  
  cbcl_external_model$performance_by_repetition |>
    dplyr::mutate(
      outcome = "cbcl_external_t5",
      .before = 1
    ),
  
  cbcl_total_model$performance_by_repetition |>
    dplyr::mutate(
      outcome = "cbcl_total_t5",
      .before = 1
    )
)

cbcl_outcome_performance_summary <-
  cbcl_outcome_performance |>
  dplyr::group_by(
    outcome
  ) |>
  dplyr::summarise(
    n_repeats = dplyr::n(),
    
    rmse_mean = mean(rmse),
    rmse_sd = stats::sd(rmse),
    
    mae_mean = mean(mae),
    mae_sd = stats::sd(mae),
    
    r_squared_mean =
      mean(r_squared),
    
    r_squared_sd =
      stats::sd(r_squared),
    
    .groups = "drop"
  )

print(
  cbcl_outcome_performance_summary,
  n = Inf,
  width = Inf
)

##### CBCL LONG-TERM STABILITY #####

cbcl_stability <- tibble::tibble(
  
  dimension = c(
    "internal",
    "external",
    "total"
  ),
  
  n_complete = c(
    sum(
      stats::complete.cases(
        amis_ml$cbcl_internal_t2,
        amis_ml$cbcl_internal_t5
      )
    ),
    
    sum(
      stats::complete.cases(
        amis_ml$cbcl_external_t2,
        amis_ml$cbcl_external_t5
      )
    ),
    
    sum(
      stats::complete.cases(
        amis_ml$cbcl_total_t2,
        amis_ml$cbcl_total_t5
      )
    )
  ),
  
  pearson_r = c(
    stats::cor(
      amis_ml$cbcl_internal_t2,
      amis_ml$cbcl_internal_t5,
      use = "complete.obs",
      method = "pearson"
    ),
    
    stats::cor(
      amis_ml$cbcl_external_t2,
      amis_ml$cbcl_external_t5,
      use = "complete.obs",
      method = "pearson"
    ),
    
    stats::cor(
      amis_ml$cbcl_total_t2,
      amis_ml$cbcl_total_t5,
      use = "complete.obs",
      method = "pearson"
    )
  ),
  
  spearman_rho = c(
    stats::cor(
      amis_ml$cbcl_internal_t2,
      amis_ml$cbcl_internal_t5,
      use = "complete.obs",
      method = "spearman"
    ),
    
    stats::cor(
      amis_ml$cbcl_external_t2,
      amis_ml$cbcl_external_t5,
      use = "complete.obs",
      method = "spearman"
    ),
    
    stats::cor(
      amis_ml$cbcl_total_t2,
      amis_ml$cbcl_total_t5,
      use = "complete.obs",
      method = "spearman"
    )
  )
)

print(
  cbcl_stability,
  n = Inf,
  width = Inf
)
##### CBCL MEAN-LEVEL CHANGE #####

cbcl_change_overview <- tibble::tibble(
  
  dimension = c(
    "internal",
    "external",
    "total"
  ),
  
  mean_t2 = c(
    mean(
      amis_ml$cbcl_internal_t2,
      na.rm = TRUE
    ),
    mean(
      amis_ml$cbcl_external_t2,
      na.rm = TRUE
    ),
    mean(
      amis_ml$cbcl_total_t2,
      na.rm = TRUE
    )
  ),
  
  mean_t5 = c(
    mean(
      amis_ml$cbcl_internal_t5,
      na.rm = TRUE
    ),
    mean(
      amis_ml$cbcl_external_t5,
      na.rm = TRUE
    ),
    mean(
      amis_ml$cbcl_total_t5,
      na.rm = TRUE
    )
  )
) |>
  dplyr::mutate(
    mean_difference =
      mean_t5 - mean_t2
  )

print(
  cbcl_change_overview,
  n = Inf,
  width = Inf
)

##### FUNCTION: PAIRED CBCL CHANGE #####

summarize_paired_change <- function(
    data,
    variable_t2,
    variable_t5,
    dimension
) {
  
  paired_data <- data |>
    dplyr::filter(
      !is.na(.data[[variable_t2]]),
      !is.na(.data[[variable_t5]])
    ) |>
    dplyr::transmute(
      score_t2 = .data[[variable_t2]],
      score_t5 = .data[[variable_t5]],
      change = score_t5 - score_t2
    )
  
  paired_test <- stats::t.test(
    paired_data$score_t5,
    paired_data$score_t2,
    paired = TRUE
  )
  
  tibble::tibble(
    dimension = dimension,
    n = nrow(paired_data),
    
    mean_t2 = mean(paired_data$score_t2),
    sd_t2 = stats::sd(paired_data$score_t2),
    
    mean_t5 = mean(paired_data$score_t5),
    sd_t5 = stats::sd(paired_data$score_t5),
    
    mean_change = mean(paired_data$change),
    sd_change = stats::sd(paired_data$change),
    
    cohen_dz =
      mean(paired_data$change) /
      stats::sd(paired_data$change),
    
    ci_lower = paired_test$conf.int[[1]],
    ci_upper = paired_test$conf.int[[2]],
    p_value = paired_test$p.value
  )
}
##### CALCULATE PAIRED CBCL CHANGE #####

cbcl_paired_change <- dplyr::bind_rows(
  
  summarize_paired_change(
    data = amis_ml,
    variable_t2 = "cbcl_internal_t2",
    variable_t5 = "cbcl_internal_t5",
    dimension = "internal"
  ),
  
  summarize_paired_change(
    data = amis_ml,
    variable_t2 = "cbcl_external_t2",
    variable_t5 = "cbcl_external_t5",
    dimension = "external"
  ),
  
  summarize_paired_change(
    data = amis_ml,
    variable_t2 = "cbcl_total_t2",
    variable_t5 = "cbcl_total_t5",
    dimension = "total"
  )
)

print(
  cbcl_paired_change,
  n = Inf,
  width = Inf
)