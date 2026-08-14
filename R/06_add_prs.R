##### LOAD ELASTIC-NET FUNCTIONS #####

source(
  here::here(
    "R",
    "05_elastic_net.R"
  )
)


##### CREATE GENETIC SUBSAMPLE #####

amis_ml_genetic <- amis_ml |>
  dplyr::filter(
    !is.na(prs_mdd),
    !is.na(genetic_pc1),
    !is.na(genetic_pc2),
    !is.na(genetic_pc3),
    !is.na(genetic_pc4)
  )

cat(
  "\nParticipants with complete genetic information:",
  nrow(amis_ml_genetic),
  "\n"
)


##### INTERNALIZING GENETIC MODELS #####

genetic_scenarios_internal <- list(
  
  # Referenzmodell in der genetischen Teilstichprobe.
  # Die PCs sind bereits enthalten, damit ausschließlich
  # der zusätzliche Nutzen des PRS geprüft wird.
  G0_clinical_pcs = c(
    model_scenarios_internal$M2_maltreatment,
    predictor_blocks$genetic_pcs
  ),
  
  G1_add_prs = c(
    model_scenarios_internal$M2_maltreatment,
    predictor_blocks$genetic_pcs,
    predictor_blocks$prs
  )
)


##### EXTERNALIZING GENETIC MODELS #####

genetic_scenarios_external <- list(
  
  G0_clinical_pcs = c(
    model_scenarios_external$M2_maltreatment,
    predictor_blocks$genetic_pcs
  ),
  
  G1_add_prs = c(
    model_scenarios_external$M2_maltreatment,
    predictor_blocks$genetic_pcs,
    predictor_blocks$prs
  )
)

nrow(amis_ml_genetic)

genetic_scenarios_internal
genetic_scenarios_external

##### RUN INTERNALIZING MODELS #####

genetic_internal_results <- run_genetic_scenarios(
  data = amis_ml_genetic,
  outcome = "sdq_internal_t5",
  scenarios = genetic_scenarios_internal
)


##### RUN EXTERNALIZING MODELS #####

genetic_external_results <- run_genetic_scenarios(
  data = amis_ml_genetic,
  outcome = "sdq_external_t5",
  scenarios = genetic_scenarios_external
)

##### COMBINE PERFORMANCE RESULTS #####

genetic_performance <- c(
  genetic_internal_results,
  genetic_external_results
) |>
  purrr::map(
    \(result) result$performance
  ) |>
  dplyr::bind_rows()


# names(genetic_performance)
# dplyr::glimpse(genetic_performance)
# nrow(genetic_performance)

##### SUMMARIZE MODEL PERFORMANCE #####

genetic_performance_summary <- genetic_performance |>
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
  genetic_performance_summary,
  n = Inf
)


##### CALCULATE INCREMENTAL VALUE OF PRS #####

prs_incremental_value <- genetic_performance |>
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
    
    # Negativ bedeutet: Der Vorhersagefehler wurde durch PRS kleiner.
    delta_rmse =
      rmse_G1_add_prs -
      rmse_G0_clinical_pcs,
    
    delta_mae =
      mae_G1_add_prs -
      mae_G0_clinical_pcs,
    
    # Positiv bedeutet: Die erklärte Varianz wurde durch PRS größer.
    delta_r_squared =
      r_squared_G1_add_prs -
      r_squared_G0_clinical_pcs
  )


##### SUMMARIZE INCREMENTAL VALUE #####

prs_incremental_summary <- prs_incremental_value |>
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
    
    prs_better_rmse_percent =
      mean(delta_rmse < 0) * 100,
    
    prs_better_r_squared_percent =
      mean(delta_r_squared > 0) * 100,
    
    .groups = "drop"
  )

print(
  prs_incremental_summary,
  n = Inf
)


