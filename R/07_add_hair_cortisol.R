##### CREATE HAIR-CORTISOL SUBSAMPLE #####

source(here::here("R", "00_setup.R"))
source(here::here("R", "06_add_prs.R"))

exists("amis_ml")
exists("run_genetic_scenarios")
exists("model_scenarios_internal")
exists("predictor_blocks")

# H0 und H1 werden auf exakt derselben Stichprobe gerechnet.
amis_ml_hcc <- amis_ml |>
  dplyr::filter(
    !is.na(hcc_t2_z)
  )

cat(
  "\nParticipants with hair-cortisol data:",
  nrow(amis_ml_hcc),
  "\n"
)


##### DEFINE INTERNALIZING HCC MODELS #####

hcc_scenarios_internal <- list(
  
  H0_clinical = c(
    model_scenarios_internal$M2_maltreatment
  ),
  
  H1_add_hcc = c(
    model_scenarios_internal$M2_maltreatment,
    predictor_blocks$hair_cortisol
  )
)


##### DEFINE EXTERNALIZING HCC MODELS #####

hcc_scenarios_external <- list(
  
  H0_clinical = c(
    model_scenarios_external$M2_maltreatment
  ),
  
  H1_add_hcc = c(
    model_scenarios_external$M2_maltreatment,
    predictor_blocks$hair_cortisol
  )
)


##### RUN INTERNALIZING HCC MODELS #####

hcc_internal_results <- run_genetic_scenarios(
  data = amis_ml_hcc,
  outcome = "sdq_internal_t5",
  scenarios = hcc_scenarios_internal
)


##### RUN EXTERNALIZING HCC MODELS #####

hcc_external_results <- run_genetic_scenarios(
  data = amis_ml_hcc,
  outcome = "sdq_external_t5",
  scenarios = hcc_scenarios_external
)

##### COMBINE HCC PERFORMANCE #####

hcc_performance <- c(
  hcc_internal_results,
  hcc_external_results
) |>
  purrr::map(
    \(result) result$performance
  ) |>
  dplyr::bind_rows()


##### SUMMARIZE HCC PERFORMANCE #####

hcc_performance_summary <- hcc_performance |>
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
  hcc_performance_summary,
  n = Inf
)


##### PAIRED INCREMENTAL VALUE #####

hcc_incremental_value <- hcc_performance |>
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
      rmse_H1_add_hcc -
      rmse_H0_clinical,
    
    delta_mae =
      mae_H1_add_hcc -
      mae_H0_clinical,
    
    delta_r_squared =
      r_squared_H1_add_hcc -
      r_squared_H0_clinical
  )


hcc_incremental_summary <- hcc_incremental_value |>
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
    
    hcc_better_rmse_percent =
      mean(delta_rmse < 0) * 100,
    
    hcc_better_r_squared_percent =
      mean(delta_r_squared > 0) * 100,
    
    .groups = "drop"
  )

print(
  hcc_incremental_summary,
  n = Inf
)
