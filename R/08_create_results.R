
source(here::here("R", "00_setup.R"))
# source(here::here("R", "06_add_prs.R"))
# source(here::here("R", "07_add_hair_cortisol.R"))

##### CHECK REQUIRED RESULTS #####

exists("crossvalidate_elastic_net")
exists("run_elastic_scenarios")
exists("amis_ml_genetic")


c(
  genetic_internal_results =
    exists("genetic_internal_results"),
  
  genetic_external_results =
    exists("genetic_external_results"),
  
  genetic_performance =
    exists("genetic_performance"),
  
  genetic_performance_summary =
    exists("genetic_performance_summary"),
  
  prs_incremental_value =
    exists("prs_incremental_value")
)



required_results <- c(
  "reference_performance",
  "reference_performance_summary",
  "elastic_net_performance_summary",
  "genetic_performance_summary",
  "prs_incremental_value",
  "hcc_performance_summary",
  "hcc_incremental_value"
)

missing_results <- required_results[
  !vapply(
    required_results,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_results) > 0) {
  stop(
    "The following results are missing: ",
    paste(missing_results, collapse = ", ")
  )
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

dir.create(
  here::here("output"),
  showWarnings = FALSE,
  recursive = TRUE
)

##### SAMPLE SIZES #####

sample_sizes <- dplyr::bind_rows(
  
  tibble::tibble(
    sample = "Full sample",
    outcome = c(
      "sdq_internal_t5",
      "sdq_external_t5"
    ),
    n = c(
      sum(!is.na(amis_ml$sdq_internal_t5)),
      sum(!is.na(amis_ml$sdq_external_t5))
    )
  ),
  
  tibble::tibble(
    sample = "Genetic subsample",
    outcome = c(
      "sdq_internal_t5",
      "sdq_external_t5"
    ),
    n = c(
      sum(!is.na(amis_ml_genetic$sdq_internal_t5)),
      sum(!is.na(amis_ml_genetic$sdq_external_t5))
    )
  ),
  
  tibble::tibble(
    sample = "HCC subsample",
    outcome = c(
      "sdq_internal_t5",
      "sdq_external_t5"
    ),
    n = c(
      sum(!is.na(amis_ml_hcc$sdq_internal_t5)),
      sum(!is.na(amis_ml_hcc$sdq_external_t5))
    )
  )
)

##### COMBINE MODEL PERFORMANCE #####

all_model_performance <- dplyr::bind_rows(
  
  reference_performance_summary |>
    dplyr::mutate(
      analysis = "Clinical model development",
      sample = "Full sample",
      method = "Linear regression"
    ),
  
  elastic_net_performance_summary |>
    dplyr::mutate(
      model = "M2_elastic_net",
      analysis = "Clinical model development",
      sample = "Full sample",
      method = "Elastic Net"
    ),
  
  genetic_performance_summary |>
    dplyr::mutate(
      analysis = "Incremental PRS analysis",
      sample = "Genetic subsample",
      method = "Elastic Net"
    ),
  
  hcc_performance_summary |>
    dplyr::mutate(
      analysis = "Incremental HCC analysis",
      sample = "HCC subsample",
      method = "Elastic Net"
    )
) |>
  dplyr::left_join(
    sample_sizes,
    by = c(
      "sample",
      "outcome"
    )
  ) |>
  dplyr::select(
    analysis,
    sample,
    outcome,
    model,
    method,
    n,
    rmse_mean,
    rmse_sd,
    mae_mean,
    mae_sd,
    r_squared_mean,
    r_squared_sd
  ) |>
  dplyr::arrange(
    analysis,
    outcome,
    model
  )

print(
  all_model_performance,
  n = Inf
)

##### CLINICAL INCREMENTAL VALUE #####

clinical_incremental_value <- reference_performance |>
  dplyr::select(
    outcome,
    model,
    repetition,
    rmse,
    r_squared
  ) |>
  tidyr::pivot_wider(
    names_from = model,
    values_from = c(
      rmse,
      r_squared
    )
  )


incremental_results <- dplyr::bind_rows(
  
  # Zusatznutzen des Ausgangs-SDQ gegenüber Demografie
  clinical_incremental_value |>
    dplyr::transmute(
      outcome,
      repetition,
      predictor_block = "Baseline SDQ",
      delta_rmse =
        rmse_M1_baseline_sdq -
        rmse_M0_demographics,
      delta_r_squared =
        r_squared_M1_baseline_sdq -
        r_squared_M0_demographics
    ),
  
  # Zusatznutzen der Misshandlungsinformationen
  clinical_incremental_value |>
    dplyr::transmute(
      outcome,
      repetition,
      predictor_block = "Maltreatment",
      delta_rmse =
        rmse_M2_maltreatment -
        rmse_M1_baseline_sdq,
      delta_r_squared =
        r_squared_M2_maltreatment -
        r_squared_M1_baseline_sdq
    ),
  
  # Zusatznutzen des PRS
  prs_incremental_value |>
    dplyr::transmute(
      outcome,
      repetition,
      predictor_block = "Polygenic risk score",
      delta_rmse,
      delta_r_squared
    ),
  
  # Zusatznutzen von Haarkortisol
  hcc_incremental_value |>
    dplyr::transmute(
      outcome,
      repetition,
      predictor_block = "Hair cortisol",
      delta_rmse,
      delta_r_squared
    )
) |>
  dplyr::mutate(
    outcome = dplyr::recode(
      outcome,
      sdq_internal_t5 = "Internalizing",
      sdq_external_t5 = "Externalizing"
    ),
    
    predictor_block = factor(
      predictor_block,
      levels = c(
        "Baseline SDQ",
        "Maltreatment",
        "Polygenic risk score",
        "Hair cortisol"
      )
    )
  )


incremental_summary <- incremental_results |>
  dplyr::group_by(
    outcome,
    predictor_block
  ) |>
  dplyr::summarise(
    delta_r_squared_mean =
      mean(delta_r_squared),
    
    delta_r_squared_sd =
      stats::sd(delta_r_squared),
    
    delta_rmse_mean =
      mean(delta_rmse),
    
    delta_rmse_sd =
      stats::sd(delta_rmse),
    
    improvement_percent =
      mean(delta_r_squared > 0) * 100,
    
    .groups = "drop"
  )

print(
  incremental_summary,
  n = Inf
)



##### PLOT INCREMENTAL PREDICTIVE VALUE #####

incremental_plot <- ggplot2::ggplot(
  incremental_results,
  ggplot2::aes(
    x = predictor_block,
    y = delta_r_squared
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    color = "grey45",
    linewidth = 0.5
  ) +
  ggplot2::geom_boxplot(
    width = 0.55,
    outlier.shape = NA,
    fill = "#DCE6F1",
    color = "#365F91"
  ) +
  ggplot2::geom_jitter(
    width = 0.08,
    height = 0,
    alpha = 0.45,
    size = 1.6,
    color = "#365F91"
  ) +
  ggplot2::stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3.5,
    color = "#C00000"
  ) +
  ggplot2::facet_wrap(
    ~ outcome,
    ncol = 1,
    scales = "free_y"
  ) +
  ggplot2::labs(
    x = NULL,
    y = expression(Delta * R^2),
    title = "Incremental out-of-sample predictive value",
    subtitle = paste(
      "Positive values indicate improved prediction;",
      "red diamonds show means"
    )
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      angle = 25,
      hjust = 1
    ),
    strip.text = ggplot2::element_text(
      face = "bold"
    ),
    plot.title = ggplot2::element_text(
      face = "bold"
    )
  )

print(incremental_plot)

utils::write.csv(
  all_model_performance,
  here::here(
    "output",
    "model_performance.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  incremental_summary,
  here::here(
    "output",
    "incremental_predictive_value.csv"
  ),
  row.names = FALSE
)

ggplot2::ggsave(
  filename = here::here(
    "output",
    "incremental_predictive_value.png"
  ),
  plot = incremental_plot,
  width = 8,
  height = 7,
  units = "in",
  dpi = 300
)

incremental_summary |>
  print(n = Inf)
