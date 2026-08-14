##### LOAD MODEL FUNCTIONS AND CBCL DATA #####


source(
  here::here(
    "R",
    "03_prepare_analysis_data.R"
  )
)

source(
  here::here(
    "R",
    "04_crossvalidate_reference_models.R"
  )
)


source(
  here::here(
    "R",
    "05_elastic_net.R"
  )
)

source(
  here::here(
    "R",
    "10_load_cbcl.R"
  )
)

##### MERGE CBCL SCORES WITH MAIN ANALYSIS DATA #####

# Sicherheitsprüfung: Pro SIC darf nur eine Zeile vorliegen.
if (anyDuplicated(cbcl_t2_scored$sic) > 0) {
  stop(
    "Duplicated SIC values remain in cbcl_t2_scored."
  )
}

if (anyDuplicated(cbcl_t5_scored$sic) > 0) {
  stop(
    "Duplicated SIC values remain in cbcl_t5_scored."
  )
}


# SIC in allen Datensätzen einheitlich formatieren.
amis_ml <- amis_ml |>
  dplyr::mutate(
    sic = stringr::str_trim(
      as.character(sic)
    )
  )

cbcl_t2_scored <- cbcl_t2_scored |>
  dplyr::mutate(
    sic = stringr::str_trim(
      as.character(sic)
    )
  )

cbcl_t5_scored <- cbcl_t5_scored |>
  dplyr::mutate(
    sic = stringr::str_trim(
      as.character(sic)
    )
  )


# T2- und T5-CBCL-Scores an den Hauptdatensatz anfügen.
amis_ml_cbcl <- amis_ml |>
  dplyr::left_join(
    cbcl_t2_scored |>
      dplyr::select(
        sic,
        cbcl_internal_t2,
        cbcl_external_t2,
        cbcl_total_t2
      ),
    by = "sic"
  ) |>
  dplyr::left_join(
    cbcl_t5_scored |>
      dplyr::select(
        sic,
        cbcl_internal_t5,
        cbcl_external_t5,
        cbcl_total_t5
      ),
    by = "sic"
  )


##### CHECK MERGE #####

stopifnot(
  nrow(amis_ml_cbcl) == nrow(amis_ml)
)

stopifnot(
  all(
    c(
      "cbcl_internal_t2",
      "cbcl_external_t2",
      "cbcl_total_t2",
      "cbcl_internal_t5",
      "cbcl_external_t5",
      "cbcl_total_t5"
    ) %in% names(amis_ml_cbcl)
  )
)

message(
  "CBCL scores successfully merged with the main analysis data."
)



##### CHECK REQUIRED OBJECTS #####

required_objects <- c(
  "amis_ml",
  "crossvalidate_elastic_net",
  "run_elastic_scenarios",
  "cbcl_scenarios_internal",
  "cbcl_scenarios_external",
  "cbcl_scenarios_total"
)

missing_objects <- required_objects[
  !vapply(
    required_objects,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_objects) > 0) {
  stop(
    "Required objects are missing: ",
    paste(
      missing_objects,
      collapse = ", "
    )
  )
}

##### CREATE CBCL SUBSAMPLES #####

amis_ml_cbcl_internal <- amis_ml |>
  dplyr::filter(
    !is.na(cbcl_internal_t2)
  )

amis_ml_cbcl_external <- amis_ml |>
  dplyr::filter(
    !is.na(cbcl_external_t2)
  )

amis_ml_cbcl_total <- amis_ml |>
  dplyr::filter(
    !is.na(cbcl_total_t2)
  )


##### PRINT SAMPLE SIZES #####

cat(
  "\nCBCL internalizing predictor sample:",
  nrow(amis_ml_cbcl_internal),
  "\n"
)

cat(
  "CBCL externalizing predictor sample:",
  nrow(amis_ml_cbcl_external),
  "\n"
)

cat(
  "CBCL total predictor sample:",
  nrow(amis_ml_cbcl_total),
  "\n"
)

##### RUN INTERNALIZING MODELS #####

cbcl_internal_results <- run_elastic_scenarios(
  data = amis_ml_cbcl_internal,
  outcome = "sdq_internal_t5",
  scenarios = cbcl_scenarios_internal
)


##### RUN EXTERNALIZING MODELS #####

cbcl_external_results <- run_elastic_scenarios(
  data = amis_ml_cbcl_external,
  outcome = "sdq_external_t5",
  scenarios = cbcl_scenarios_external
)


##### RUN TOTAL-DIFFICULTIES MODELS #####

cbcl_total_results <- run_elastic_scenarios(
  data = amis_ml_cbcl_total,
  outcome = "sdq_total_t5",
  scenarios = cbcl_scenarios_total
)
##### COMBINE CBCL PERFORMANCE #####

cbcl_performance <- c(
  cbcl_internal_results,
  cbcl_external_results,
  cbcl_total_results
) |>
  purrr::map(
    \(result) result$performance
  ) |>
  dplyr::bind_rows()


##### SUMMARIZE CBCL PERFORMANCE #####

cbcl_performance_summary <- cbcl_performance |>
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
    model
  )

print(
  cbcl_performance_summary,
  n = Inf
)

##### CALCULATE INCREMENTAL CBCL VALUE #####

cbcl_incremental_value <- cbcl_performance |>
  dplyr::select(
    outcome,
    model,
    repetition,
    rmse,
    mae,
    r_squared
  ) |>
  tidyr::pivot_wider(
    names_from = model,
    values_from = c(
      rmse,
      mae,
      r_squared
    )
  ) |>
  dplyr::mutate(
    delta_rmse =
      rmse_C1_add_cbcl -
      rmse_C0_clinical,
    
    delta_mae =
      mae_C1_add_cbcl -
      mae_C0_clinical,
    
    delta_r_squared =
      r_squared_C1_add_cbcl -
      r_squared_C0_clinical
  )


##### SUMMARIZE INCREMENTAL CBCL VALUE #####

cbcl_incremental_summary <- cbcl_incremental_value |>
  dplyr::group_by(outcome) |>
  dplyr::summarise(
    delta_rmse_mean = mean(delta_rmse),
    delta_rmse_sd = stats::sd(delta_rmse),
    
    delta_mae_mean = mean(delta_mae),
    delta_mae_sd = stats::sd(delta_mae),
    
    delta_r_squared_mean =
      mean(delta_r_squared),
    
    delta_r_squared_sd =
      stats::sd(delta_r_squared),
    
    cbcl_better_rmse_percent =
      mean(delta_rmse < 0) * 100,
    
    cbcl_better_r_squared_percent =
      mean(delta_r_squared > 0) * 100,
    
    .groups = "drop"
  )

print(
  cbcl_incremental_summary,
  n = Inf,
  width = Inf
)
