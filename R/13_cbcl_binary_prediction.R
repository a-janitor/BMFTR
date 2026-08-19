##### BINARY LONG-TERM CBCL PREDICTION #####

# Prerequisite: Run R/11_add_cbcl.R first.

if (!exists("amis_ml_cbcl")) {
  stop(
    "Object 'amis_ml_cbcl' is missing. Run R/11_add_cbcl.R first."
  )
}

if (!requireNamespace("glmnet", quietly = TRUE)) {
  stop("Package 'glmnet' is required.")
}


##### CREATE A DYNAMIC BINARY CBCL OUTCOME #####

create_cbcl_binary_outcome <- function(
    data,
    dimension = c("total", "internal", "external"),
    timepoint = "t5",
    cutoff = 64
) {
  dimension <- match.arg(dimension)

  t_variable <- paste0(
    "cbcl_",
    dimension,
    "_",
    timepoint,
    "_t"
  )

  if (!t_variable %in% names(data)) {
    stop(
      "T-score variable not found: ",
      t_variable
    )
  }

  outcome_name <- paste0(
    "cbcl_",
    dimension,
    "_",
    timepoint,
    "_ge_",
    cutoff
  )

  data[[outcome_name]] <- dplyr::if_else(
    is.na(data[[t_variable]]),
    NA_integer_,
    as.integer(data[[t_variable]] >= cutoff)
  )

  list(
    data = data,
    outcome = outcome_name,
    t_variable = t_variable,
    cutoff = cutoff,
    dimension = dimension,
    timepoint = timepoint
  )
}


##### STRATIFIED FOLD ASSIGNMENT #####

make_stratified_folds <- function(
    outcome,
    v = 10
) {
  fold_id <- integer(length(outcome))

  for (outcome_value in sort(unique(outcome))) {
    indices <- which(outcome == outcome_value)
    indices <- sample(indices)

    fold_id[indices] <- rep(
      seq_len(v),
      length.out = length(indices)
    )
  }

  fold_id
}


##### BINARY PERFORMANCE METRICS #####

binary_roc_auc <- function(
    truth,
    probability
) {
  n_positive <- sum(truth == 1)
  n_negative <- sum(truth == 0)

  if (n_positive == 0 || n_negative == 0) {
    return(NA_real_)
  }

  ranks <- rank(
    probability,
    ties.method = "average"
  )

  (
    sum(ranks[truth == 1]) -
      n_positive * (n_positive + 1) / 2
  ) / (n_positive * n_negative)
}


binary_average_precision <- function(
    truth,
    probability
) {
  n_positive <- sum(truth == 1)

  if (n_positive == 0) {
    return(NA_real_)
  }

  ordering <- order(
    probability,
    decreasing = TRUE
  )

  sorted_truth <- truth[ordering]
  cumulative_precision <- cumsum(sorted_truth) /
    seq_along(sorted_truth)

  sum(
    cumulative_precision[sorted_truth == 1]
  ) / n_positive
}


calculate_binary_metrics <- function(
    truth,
    probability,
    classification_threshold = 0.50
) {
  probability <- pmin(
    pmax(probability, 1e-6),
    1 - 1e-6
  )

  predicted_class <- as.integer(
    probability >= classification_threshold
  )

  true_positive <- sum(
    predicted_class == 1 & truth == 1
  )
  true_negative <- sum(
    predicted_class == 0 & truth == 0
  )
  false_positive <- sum(
    predicted_class == 1 & truth == 0
  )
  false_negative <- sum(
    predicted_class == 0 & truth == 1
  )

  sensitivity <- if (
    true_positive + false_negative > 0
  ) {
    true_positive /
      (true_positive + false_negative)
  } else {
    NA_real_
  }

  specificity <- if (
    true_negative + false_positive > 0
  ) {
    true_negative /
      (true_negative + false_positive)
  } else {
    NA_real_
  }

  tibble::tibble(
    n = length(truth),
    n_events = sum(truth == 1),
    prevalence = mean(truth),
    roc_auc = binary_roc_auc(
      truth,
      probability
    ),
    pr_auc = binary_average_precision(
      truth,
      probability
    ),
    brier = mean(
      (probability - truth)^2
    ),
    log_loss = -mean(
      truth * log(probability) +
        (1 - truth) * log(1 - probability)
    ),
    sensitivity = sensitivity,
    specificity = specificity,
    balanced_accuracy = mean(
      c(
        sensitivity,
        specificity
      ),
      na.rm = TRUE
    )
  )
}


##### NESTED REPEATED CROSS-VALIDATION #####

crossvalidate_binary_elastic_net <- function(
    data,
    outcome,
    predictors,
    outer_folds = 10,
    repeats = 10,
    inner_folds = 5,
    alpha_grid = seq(0, 1, by = 0.25),
    seed = 20260819
) {
  required_variables <- unique(
    c(
      outcome,
      predictors
    )
  )

  missing_variables <- setdiff(
    required_variables,
    names(data)
  )

  if (length(missing_variables) > 0) {
    stop(
      "Variables missing from data: ",
      paste(
        missing_variables,
        collapse = ", "
      )
    )
  }

  analysis_data <- data |>
    dplyr::select(
      dplyr::all_of(required_variables)
    ) |>
    tidyr::drop_na()

  truth <- as.integer(
    analysis_data[[outcome]]
  )

  if (!all(truth %in% c(0L, 1L))) {
    stop("Outcome must be coded 0/1.")
  }

  n_events <- sum(truth == 1)
  n_non_events <- sum(truth == 0)

  if (min(n_events, n_non_events) < outer_folds) {
    stop(
      "The smaller outcome class contains fewer observations than outer_folds."
    )
  }

  design_matrix <- stats::model.matrix(
    ~ .,
    data = analysis_data[
      predictors
    ]
  )[
    ,
    -1,
    drop = FALSE
  ]

  set.seed(seed)

  predictions <- vector(
    "list",
    length = repeats * outer_folds
  )

  result_index <- 1L

  for (repeat_id in seq_len(repeats)) {
    outer_fold_id <- make_stratified_folds(
      outcome = truth,
      v = outer_folds
    )

    for (fold in seq_len(outer_folds)) {
      test_indices <- which(
        outer_fold_id == fold
      )
      training_indices <- which(
        outer_fold_id != fold
      )

      training_outcome <- truth[training_indices]
      inner_fold_id <- make_stratified_folds(
        outcome = training_outcome,
        v = inner_folds
      )

      candidate_models <- lapply(
        alpha_grid,
        function(alpha_value) {
          glmnet::cv.glmnet(
            x = design_matrix[
              training_indices,
              ,
              drop = FALSE
            ],
            y = training_outcome,
            family = "binomial",
            alpha = alpha_value,
            foldid = inner_fold_id,
            type.measure = "deviance",
            standardize = TRUE
          )
        }
      )

      candidate_deviance <- vapply(
        candidate_models,
        function(model) {
          min(
            model$cvm,
            na.rm = TRUE
          )
        },
        numeric(1)
      )

      best_index <- which.min(
        candidate_deviance
      )
      best_model <- candidate_models[[best_index]]

      fold_probability <- as.numeric(
        stats::predict(
          best_model,
          newx = design_matrix[
            test_indices,
            ,
            drop = FALSE
          ],
          s = "lambda.min",
          type = "response"
        )
      )

      predictions[[result_index]] <- tibble::tibble(
        repeat_id = repeat_id,
        fold = fold,
        row_id = test_indices,
        truth = truth[test_indices],
        probability = fold_probability,
        selected_alpha = alpha_grid[[best_index]],
        selected_lambda = best_model$lambda.min
      )

      result_index <- result_index + 1L
    }
  }

  predictions <- dplyr::bind_rows(
    predictions
  )

  metrics_by_repeat <- predictions |>
    dplyr::group_by(
      repeat_id
    ) |>
    dplyr::group_modify(
      ~ calculate_binary_metrics(
        truth = .x$truth,
        probability = .x$probability
      )
    ) |>
    dplyr::ungroup()

  performance_summary <- metrics_by_repeat |>
    dplyr::summarise(
      dplyr::across(
        c(
          prevalence,
          roc_auc,
          pr_auc,
          brier,
          log_loss,
          sensitivity,
          specificity,
          balanced_accuracy
        ),
        list(
          mean = mean,
          sd = stats::sd
        ),
        na.rm = TRUE
      )
    )

  list(
    outcome = outcome,
    predictors = predictors,
    n = nrow(analysis_data),
    n_events = n_events,
    predictions = predictions,
    metrics_by_repeat = metrics_by_repeat,
    performance_summary = performance_summary
  )
}


##### BUILD DYNAMIC PREDICTOR SCENARIOS #####

build_cbcl_binary_scenarios <- function(
    dimension = c("total", "internal", "external")
) {
  dimension <- match.arg(dimension)

  scenario_object <- get(
    paste0(
      "model_scenarios_",
      dimension
    ),
    inherits = TRUE
  )

  cbcl_baseline <- paste0(
    "cbcl_",
    dimension,
    "_t2"
  )

  demographics <- scenario_object$M0_demographics

  maltreatment_increment <- setdiff(
    scenario_object$M2_maltreatment,
    scenario_object$M1_baseline
  )

  list(
    B0_demographics = demographics,
    B1_add_cbcl = unique(
      c(
        demographics,
        cbcl_baseline
      )
    ),
    B2_add_maltreatment = unique(
      c(
        demographics,
        cbcl_baseline,
        maltreatment_increment
      )
    ),
    B3_add_sdq = unique(
      c(
        scenario_object$M2_maltreatment,
        cbcl_baseline
      )
    )
  )
}


##### RUN SCENARIOS ON A COMMON ANALYTIC SAMPLE #####

run_cbcl_binary_scenarios <- function(
    data,
    dimension = c("total", "internal", "external"),
    cutoff = 64,
    outer_folds = 10,
    repeats = 10,
    inner_folds = 5,
    alpha_grid = seq(0, 1, by = 0.25),
    seed = 20260819
) {
  dimension <- match.arg(dimension)

  outcome_setup <- create_cbcl_binary_outcome(
    data = data,
    dimension = dimension,
    timepoint = "t5",
    cutoff = cutoff
  )

  scenarios <- build_cbcl_binary_scenarios(
    dimension = dimension
  )

  all_predictors <- unique(
    unlist(
      scenarios,
      use.names = FALSE
    )
  )

  common_data <- outcome_setup$data |>
    dplyr::select(
      dplyr::all_of(
        c(
          outcome_setup$outcome,
          all_predictors
        )
      )
    ) |>
    tidyr::drop_na()

  results <- lapply(
    names(scenarios),
    function(scenario_name) {
      result <- crossvalidate_binary_elastic_net(
        data = common_data,
        outcome = outcome_setup$outcome,
        predictors = scenarios[[scenario_name]],
        outer_folds = outer_folds,
        repeats = repeats,
        inner_folds = inner_folds,
        alpha_grid = alpha_grid,
        seed = seed
      )

      result$scenario <- scenario_name
      result
    }
  )

  names(results) <- names(scenarios)

  performance <- dplyr::bind_rows(
    lapply(
      results,
      function(result) {
        result$performance_summary |>
          dplyr::mutate(
            scenario = result$scenario,
            n = result$n,
            n_events = result$n_events,
            .before = 1
          )
      }
    )
  )

  list(
    dimension = dimension,
    cutoff = cutoff,
    outcome = outcome_setup$outcome,
    scenarios = scenarios,
    results = results,
    performance = performance
  )
}


##### PRIMARY ANALYSIS: TOTAL PROBLEMS, T >= 64 #####

cbcl_binary_total_64 <- run_cbcl_binary_scenarios(
  data = amis_ml_cbcl,
  dimension = "total",
  cutoff = 64,
  outer_folds = 10,
  repeats = 10,
  inner_folds = 5,
  alpha_grid = seq(0, 1, by = 0.25),
  seed = 20260819
)

print(
  cbcl_binary_total_64$performance,
  n = Inf,
  width = Inf
)


##### OPTIONAL SECONDARY ANALYSES #####

# Internalizing clinical-range outcome:
cbcl_binary_internal_64 <- run_cbcl_binary_scenarios(
  data = amis_ml_cbcl,
  dimension = "internal",
  cutoff = 64
)

# Total borderline-or-clinical outcome:
cbcl_binary_total_60 <- run_cbcl_binary_scenarios(
  data = amis_ml_cbcl,
  dimension = "total",
  cutoff = 60
)

##### SECONDARY BINARY CBCL MODELS #####

cbcl_binary_internal_64 <- run_cbcl_binary_scenarios(
  data = amis_ml_cbcl,
  dimension = "internal",
  cutoff = 64,
  outer_folds = 10,
  repeats = 10,
  inner_folds = 5,
  alpha_grid = seq(
    0,
    1,
    by = 0.25
  ),
  seed = 20260819
)

cbcl_binary_total_60 <- run_cbcl_binary_scenarios(
  data = amis_ml_cbcl,
  dimension = "total",
  cutoff = 60,
  outer_folds = 10,
  repeats = 10,
  inner_folds = 5,
  alpha_grid = seq(
    0,
    1,
    by = 0.25
  ),
  seed = 20260819
)

print(
  cbcl_binary_internal_64$performance,
  n = Inf,
  width = Inf
)

print(
  cbcl_binary_total_60$performance,
  n = Inf,
  width = Inf
)

##### EVALUATE CLASSIFICATION THRESHOLDS #####

evaluate_probability_thresholds <- function(
    binary_analysis,
    scenario = "B1_add_cbcl",
    thresholds = seq(
      0.05,
      0.50,
      by = 0.025
    )
) {
  
  predictions <- binary_analysis$
    results[[scenario]]$
    predictions
  
  threshold_results <- lapply(
    sort(unique(predictions$repeat_id)),
    function(repeat_value) {
      
      repeat_predictions <- predictions |>
        dplyr::filter(
          repeat_id == repeat_value
        )
      
      dplyr::bind_rows(
        lapply(
          thresholds,
          function(threshold_value) {
            
            calculate_binary_metrics(
              truth = repeat_predictions$truth,
              probability =
                repeat_predictions$probability,
              classification_threshold =
                threshold_value
            ) |>
              dplyr::mutate(
                repeat_id = repeat_value,
                threshold = threshold_value,
                .before = 1
              )
          }
        )
      )
    }
  ) |>
    dplyr::bind_rows()
  
  threshold_summary <- threshold_results |>
    dplyr::group_by(
      threshold
    ) |>
    dplyr::summarise(
      dplyr::across(
        c(
          sensitivity,
          specificity,
          balanced_accuracy
        ),
        list(
          mean = mean,
          sd = stats::sd
        ),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      youden_j =
        sensitivity_mean +
        specificity_mean -
        1
    )
  
  list(
    results_by_repeat = threshold_results,
    summary = threshold_summary
  )
}




total_64_thresholds <- evaluate_probability_thresholds(
  binary_analysis = cbcl_binary_total_64,
  scenario = "B1_add_cbcl"
)

print(
  total_64_thresholds$summary,
  n = Inf,
  width = Inf
)

total_64_best_youden <- total_64_thresholds$summary |>
    dplyr::slice_max(
      order_by = youden_j,
      n = 1,
      with_ties = FALSE
    )
  
total_64_best_youden

total_64_sensitivity_80 <- total_64_thresholds$summary |>
  dplyr::filter(
    sensitivity_mean >= 0.80
  ) |>
  dplyr::slice_max(
    order_by = specificity_mean,
    n = 1,
    with_ties = FALSE
  )

total_64_sensitivity_80

internal_64_thresholds <- evaluate_probability_thresholds(
  cbcl_binary_internal_64,
  scenario = "B1_add_cbcl"
)

total_60_thresholds <- evaluate_probability_thresholds(
  cbcl_binary_total_60,
  scenario = "B1_add_cbcl"
)


internal_64_thresholds_b2 <- evaluate_probability_thresholds(
  binary_analysis = cbcl_binary_internal_64,
  scenario = "B2_add_maltreatment"
)

internal_64_best_youden <- internal_64_thresholds_b2$summary |>
  dplyr::slice_max(
    order_by = youden_j,
    n = 1,
    with_ties = FALSE
  )

internal_64_sensitivity_80 <- internal_64_thresholds_b2$summary |>
  dplyr::filter(
    sensitivity_mean >= 0.80
  ) |>
  dplyr::slice_max(
    order_by = specificity_mean,
    n = 1,
    with_ties = FALSE
  )

internal_64_best_youden
internal_64_sensitivity_80


compare_binary_scenarios <- function(
    binary_analysis,
    reference = "B1_add_cbcl",
    comparison = "B2_add_maltreatment"
) {
  
  reference_metrics <- binary_analysis$
    results[[reference]]$
    metrics_by_repeat |>
    dplyr::select(
      repeat_id,
      dplyr::all_of(
        c(
          "roc_auc",
          "pr_auc",
          "brier",
          "log_loss"
        )
      )
    )
  
  comparison_metrics <- binary_analysis$
    results[[comparison]]$
    metrics_by_repeat |>
    dplyr::select(
      repeat_id,
      dplyr::all_of(
        c(
          "roc_auc",
          "pr_auc",
          "brier",
          "log_loss"
        )
      )
    )
  
  dplyr::left_join(
    reference_metrics,
    comparison_metrics,
    by = "repeat_id",
    suffix = c("_reference", "_comparison")
  ) |>
    dplyr::summarise(
      delta_roc_auc_mean =
        mean(roc_auc_comparison - roc_auc_reference),
      delta_roc_auc_sd =
        sd(roc_auc_comparison - roc_auc_reference),
      
      delta_pr_auc_mean =
        mean(pr_auc_comparison - pr_auc_reference),
      delta_pr_auc_sd =
        sd(pr_auc_comparison - pr_auc_reference),
      
      delta_brier_mean =
        mean(brier_comparison - brier_reference),
      delta_brier_sd =
        sd(brier_comparison - brier_reference),
      
      delta_log_loss_mean =
        mean(log_loss_comparison - log_loss_reference),
      delta_log_loss_sd =
        sd(log_loss_comparison - log_loss_reference)
    )
}

internal_increment <- compare_binary_scenarios(
  cbcl_binary_internal_64
)

internal_increment


##### BIOMARKERS ######
biomarker_variables <- names(amis_ml_cbcl)[
  grepl(
    "prs|genetic_pc|hcc|cortisol",
    names(amis_ml_cbcl),
    ignore.case = TRUE
  )
]

biomarker_variables

##### DEFINE BIOMARKER BLOCKS DYNAMICALLY #####

genetic_pc_predictors <- names(amis_ml_cbcl)[
  grepl(
    "genetic_pc",
    names(amis_ml_cbcl),
    ignore.case = TRUE
  )
]

prs_predictors <- names(amis_ml_cbcl)[
  grepl(
    "prs",
    names(amis_ml_cbcl),
    ignore.case = TRUE
  )
]

hcc_predictors <- names(amis_ml_cbcl)[
  grepl(
    "hcc|hair.*cortisol",
    names(amis_ml_cbcl),
    ignore.case = TRUE
  )
]

genetic_pc_predictors
prs_predictors
hcc_predictors

stopifnot(
  length(genetic_pc_predictors) > 0,
  length(prs_predictors) > 0,
  length(hcc_predictors) > 0
)

##### RUN CUSTOM BINARY SCENARIOS #####

run_custom_binary_scenarios <- function(
    data,
    dimension = "internal",
    cutoff = 64,
    scenarios,
    outer_folds = 10,
    repeats = 10,
    inner_folds = 5,
    alpha_grid = seq(
      0,
      1,
      by = 0.25
    ),
    seed = 20260819
) {
  
  outcome_setup <- create_cbcl_binary_outcome(
    data = data,
    dimension = dimension,
    timepoint = "t5",
    cutoff = cutoff
  )
  
  all_predictors <- unique(
    unlist(
      scenarios,
      use.names = FALSE
    )
  )
  
  common_data <- outcome_setup$data |>
    dplyr::select(
      dplyr::all_of(
        c(
          outcome_setup$outcome,
          all_predictors
        )
      )
    ) |>
    tidyr::drop_na()
  
  results <- lapply(
    names(scenarios),
    function(scenario_name) {
      
      result <- crossvalidate_binary_elastic_net(
        data = common_data,
        outcome = outcome_setup$outcome,
        predictors = scenarios[[scenario_name]],
        outer_folds = outer_folds,
        repeats = repeats,
        inner_folds = inner_folds,
        alpha_grid = alpha_grid,
        seed = seed
      )
      
      result$scenario <- scenario_name
      result
    }
  )
  
  names(results) <- names(scenarios)
  
  performance <- dplyr::bind_rows(
    lapply(
      results,
      function(result) {
        
        result$performance_summary |>
          dplyr::mutate(
            scenario = result$scenario,
            n = result$n,
            n_events = result$n_events,
            .before = 1
          )
      }
    )
  )
  
  list(
    dimension = dimension,
    cutoff = cutoff,
    outcome = outcome_setup$outcome,
    scenarios = scenarios,
    results = results,
    performance = performance
  )
}

base_predictors <- cbcl_binary_internal_64$
  scenarios$
  B2_add_maltreatment

prs_scenarios <- list(
  
  G0_base = base_predictors,
  
  G1_add_genetic_pcs = unique(
    c(
      base_predictors,
      genetic_pc_predictors
    )
  ),
  
  G2_add_prs = unique(
    c(
      base_predictors,
      genetic_pc_predictors,
      prs_predictors
    )
  )
)

cbcl_binary_internal_prs <- run_custom_binary_scenarios(
  data = amis_ml_cbcl,
  dimension = "internal",
  cutoff = 64,
  scenarios = prs_scenarios
)

print(
  cbcl_binary_internal_prs$performance,
  n = Inf,
  width = Inf
)

hcc_scenarios <- list(
  
  H0_base = base_predictors,
  
  H1_add_hcc = unique(
    c(
      base_predictors,
      hcc_predictors
    )
  )
)

cbcl_binary_internal_hcc <- run_custom_binary_scenarios(
  data = amis_ml_cbcl,
  dimension = "internal",
  cutoff = 64,
  scenarios = hcc_scenarios
)

print(
  cbcl_binary_internal_hcc$performance,
  n = Inf,
  width = Inf
)

joint_biomarker_scenarios <- list(
  
  J0_base = base_predictors,
  
  J1_add_genetic_pcs = unique(
    c(
      base_predictors,
      genetic_pc_predictors
    )
  ),
  
  J2_add_prs = unique(
    c(
      base_predictors,
      genetic_pc_predictors,
      prs_predictors
    )
  ),
  
  J3_add_hcc = unique(
    c(
      base_predictors,
      hcc_predictors
    )
  ),
  
  J4_add_all = unique(
    c(
      base_predictors,
      genetic_pc_predictors,
      prs_predictors,
      hcc_predictors
    )
  )
)

cbcl_binary_internal_biomarkers <- run_custom_binary_scenarios(
  data = amis_ml_cbcl,
  dimension = "internal",
  cutoff = 64,
  scenarios = joint_biomarker_scenarios
)

print(
  cbcl_binary_internal_biomarkers$performance,
  n = Inf,
  width = Inf
)



prs_increment_genetic_sample <- compare_binary_scenarios(
  binary_analysis = cbcl_binary_internal_prs,
  reference = "G1_add_genetic_pcs",
  comparison = "G2_add_prs"
)

hcc_increment <- compare_binary_scenarios(
  binary_analysis = cbcl_binary_internal_hcc,
  reference = "H0_base",
  comparison = "H1_add_hcc"
)

prs_increment_joint_sample <- compare_binary_scenarios(
  binary_analysis = cbcl_binary_internal_biomarkers,
  reference = "J1_add_genetic_pcs",
  comparison = "J2_add_prs"
)

hcc_increment_after_prs <- compare_binary_scenarios(
  binary_analysis = cbcl_binary_internal_biomarkers,
  reference = "J2_add_prs",
  comparison = "J4_add_all"
)

prs_increment_genetic_sample
hcc_increment
prs_increment_joint_sample
hcc_increment_after_prs

##### EDITORIAL AUC MODEL-COMPARISON PLOT #####

library(ggplot2)
library(tibble)
library(dplyr)
library(scales)


##### PREPARE DATA #####

auc_plot_data <- tibble(
  model_number = 1:4,
  
  model = factor(
    c(
      "Demografie",
      "+ frühe CBCL",
      "+ Misshandlung",
      "+ SDQ"
    ),
    levels = c(
      "Demografie",
      "+ frühe CBCL",
      "+ Misshandlung",
      "+ SDQ"
    )
  ),
  
  auc = c(
    0.550,
    0.785,
    0.797,
    0.797
  ),
  
  model_type = c(
    "Referenzmodell",
    "Psychopathologie",
    "Misshandlung",
    "Zusätzlicher SDQ"
  )
) |>
  mutate(
    auc_label = sprintf(
      "%.3f",
      auc
    ),
    
    previous_auc = lag(
      auc
    ),
    
    delta_auc =
      auc - previous_auc,
    
    delta_label = if_else(
      is.na(delta_auc),
      NA_character_,
      paste0(
        "\u0394 ",
        sprintf(
          "%+.3f",
          delta_auc
        )
      )
    ),
    
    label_x =
      model_number - 0.5,
    
    label_y = (
      auc +
        previous_auc
    ) / 2
  )


##### CREATE FIGURE #####

auc_plot <- ggplot(
  auc_plot_data,
  aes(
    x = model_number,
    y = auc
  )
) +
  
  annotate(
    geom = "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = 0.50,
    ymax = 0.60,
    fill = "#F3F4F6",
    alpha = 0.75
  ) +
  
  geom_hline(
    yintercept = 0.50,
    linewidth = 0.6,
    linetype = "dashed",
    color = "#8A8F98"
  ) +
  
  geom_line(
    aes(
      group = 1
    ),
    linewidth = 1.2,
    color = "#CBD0D6"
  ) +
  
  geom_point(
    aes(
      fill = model_type
    ),
    shape = 21,
    size = 5.2,
    stroke = 1.1,
    color = "white"
  ) +
  
  geom_text(
    aes(
      label = auc_label
    ),
    vjust = -1.25,
    fontface = "bold",
    size = 4.2,
    color = "#1F2933"
  ) +
  
  geom_text(
    data = auc_plot_data |>
      filter(
        !is.na(delta_label)
      ),
    aes(
      x = label_x,
      y = label_y,
      label = delta_label
    ),
    inherit.aes = FALSE,
    size = 3.4,
    color = "#68717D"
  ) +
  
  annotate(
    geom = "text",
    x = 0.72,
    y = 0.515,
    label = "Zufallsniveau",
    hjust = 0,
    size = 3.2,
    color = "#737A84"
  ) +
  
  scale_x_continuous(
    breaks = auc_plot_data$model_number,
    labels = levels(
      auc_plot_data$model
    ),
    expand = expansion(
      mult = c(
        0.08,
        0.08
      )
    )
  ) +
  
  scale_y_continuous(
    limits = c(
      0.50,
      0.835
    ),
    breaks = seq(
      0.50,
      0.80,
      by = 0.05
    ),
    labels = number_format(
      accuracy = 0.01
    ),
    expand = expansion(
      mult = c(
        0,
        0.02
      )
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Referenzmodell" = "#A7AFB8",
      "Psychopathologie" = "#276A8F",
      "Misshandlung" = "#C76D3A",
      "Zusätzlicher SDQ" = "#758A75"
    )
  ) +
  
  labs(
    title = paste0(
      "Vorhersage klinisch relevanter ",
      "internalisierender Probleme"
    ),
    
    subtitle = paste0(
      "Der frühe CBCL trägt den größten Teil der ",
      "langfristigen Vorhersageleistung"
    ),
    
    x = NULL,
    y = "Kreuzvalidierte ROC-AUC",
    
    caption = paste0(
      "Outcome: CBCL internalisierend T \u2265 64 ",
      "nach etwa acht Jahren \u00b7 N = 548\n",
      "10 \u00dd7 wiederholte stratifizierte ",
      "10-Fold-Cross-Validation"
    ),
    
    fill = NULL
  ) +
  
  guides(
    fill = "none"
  ) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  theme_minimal(
    base_family = "sans",
    base_size = 12
  ) +
  
  theme(
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.major.y = element_line(
      color = "#E8EAED",
      linewidth = 0.45
    ),
    
    axis.text.x = element_text(
      color = "#30343B",
      size = 10.5,
      margin = margin(
        t = 8
      )
    ),
    
    axis.text.y = element_text(
      color = "#68717D"
    ),
    
    axis.title.y = element_text(
      color = "#30343B",
      margin = margin(
        r = 10
      )
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      color = "#18212B",
      margin = margin(
        b = 5
      )
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      color = "#59636E",
      margin = margin(
        b = 18
      )
    ),
    
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      lineheight = 1.25,
      color = "#68717D",
      margin = margin(
        t = 14
      )
    ),
    
    plot.margin = margin(
      16,
      24,
      16,
      16
    )
  )


##### DISPLAY FIGURE #####

auc_plot


##### SAVE FIGURE #####

dir.create(
  "Results",
  showWarnings = FALSE,
  recursive = TRUE
)

ggsave(
  filename = file.path(
    "Results",
    "cbcl_internal_auc_model_comparison.png"
  ),
  plot = auc_plot,
  width = 9,
  height = 5.7,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  filename = file.path(
    "Results",
    "cbcl_internal_auc_model_comparison.pdf"
  ),
  plot = auc_plot,
  width = 9,
  height = 5.7,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)