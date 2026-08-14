##### LOAD VARIABLE DEFINITIONS #####

# Lädt Setup, Hauptdatensatz und Variablenübersicht.
source(
  here::here(
    "R",
    "02_define_variables.R"
  )
)

##### DEFINE INDIVIDUAL VARIABLES #####

# Soziodemografische Variablen
var_sex <- "sdq_sex"
var_age <- "sdq_age_parent_t2"
var_maternal_education <- "ses_ausb1_m_combined"


# SDQ: Elternurteil zu T2
var_sdq_emotion_t2 <- "sdq_emotion_b_t2"
var_sdq_peer_t2 <- "sdq_peer_b_t2"
var_sdq_conduct_t2 <- "sdq_conduct_b_t2"
var_sdq_hyper_t2 <- "sdq_hyper_b_t2"


# SDQ: Elternurteil zu T5
var_sdq_emotion_t5 <- "sdq_emotion_b_t5"
var_sdq_peer_t5 <- "sdq_peer_b_t5"
var_sdq_conduct_t5 <- "sdq_conduct_b_t5"
var_sdq_hyper_t5 <- "sdq_hyper_b_t5"


# Misshandlungsindikatoren aus dem frühen Messzeitraum.
# Wir verwenden zunächst drei kompakte Belastungsdimensionen.
var_mt_subtypes <- "mt_n_subtypes_5_t1"
var_mt_frequency <- "mt_frequency_t1"
var_mt_severity <- "mt_severity_t1"

##### GENETIC VARIABLES #####

var_prs_mdd <-
  "cortgen_mdd_multi_anc_no23_adams2025_pst_eff_a1_b0_5_phiauto"

var_genetic_pcs <- c(
  "cortgen_pc1",
  "cortgen_pc2",
  "cortgen_pc3",
  "cortgen_pc4"
)

##### HAIRCORT #####

var_hcc_t2 <- "cortgen_cort_t2_pg1_z"


##### CHECK WHETHER VARIABLES EXIST #####

required_variables <- c(
  var_sex,
  var_age,
  var_maternal_education,
  var_sdq_emotion_t2,
  var_sdq_peer_t2,
  var_sdq_conduct_t2,
  var_sdq_hyper_t2,
  var_sdq_emotion_t5,
  var_sdq_peer_t5,
  var_sdq_conduct_t5,
  var_sdq_hyper_t5,
  var_mt_subtypes,
  var_mt_frequency,
  var_mt_severity,
  var_prs_mdd,
  var_genetic_pcs,
  var_hcc_t2
)

missing_variables <- setdiff(
  required_variables,
  names(amis_ml_raw)
)

if (length(missing_variables) > 0) {
  
  stop(
    "The following variables were not found:\n",
    paste(
      missing_variables,
      collapse = "\n"
    )
  )
}

message("All required variables were found.")

##### CREATE ANALYSIS DATA #####

amis_ml <- amis_ml_raw |>
  
  # Zunächst werden nur die benötigten Variablen übernommen.
  dplyr::transmute(
    
    # Temporäre eindeutige Zeilennummer.
    # Die tatsächliche Personen-ID ergänzen wir später.
    row_id = dplyr::row_number(),
    
    sic = stringr::str_trim(
      as.character(sic)
    ),
    
    # Demografie
    sex = .data[[var_sex]],
    age_t2 = .data[[var_age]],
    maternal_education = .data[[var_maternal_education]],
    
    # SDQ zu T2
    sdq_emotion_t2 = .data[[var_sdq_emotion_t2]],
    sdq_peer_t2 = .data[[var_sdq_peer_t2]],
    sdq_conduct_t2 = .data[[var_sdq_conduct_t2]],
    sdq_hyper_t2 = .data[[var_sdq_hyper_t2]],
    
    # SDQ zu T5
    sdq_emotion_t5 = .data[[var_sdq_emotion_t5]],
    sdq_peer_t5 = .data[[var_sdq_peer_t5]],
    sdq_conduct_t5 = .data[[var_sdq_conduct_t5]],
    sdq_hyper_t5 = .data[[var_sdq_hyper_t5]],
    
    # Misshandlung
    mt_subtypes_t1 = .data[[var_mt_subtypes]],
    mt_frequency_t1 = .data[[var_mt_frequency]],
    mt_severity_t1 = .data[[var_mt_severity]],
    
    # Genetik
    prs_mdd = .data[[var_prs_mdd]],
    genetic_pc1 = .data[["cortgen_pc1"]],
    genetic_pc2 = .data[["cortgen_pc2"]],
    genetic_pc3 = .data[["cortgen_pc3"]],
    genetic_pc4 = .data[["cortgen_pc4"]],
    
    # CORTISOL
    hcc_t2_z = .data[[var_hcc_t2]]
    
  ) |>
  
  # Geschlecht und Bildung sind kategoriale Variablen.
  dplyr::mutate(
    sex = factor(sex),
    maternal_education = factor(maternal_education)
  )

##### CREATE SDQ COMPOSITE SCORES #####

amis_ml <- amis_ml |>
  dplyr::mutate(
    
    # Internalisierende Probleme:
    # emotionale Probleme + Probleme mit Gleichaltrigen
    sdq_internal_t2 = dplyr::if_else(
      !is.na(sdq_emotion_t2) &
        !is.na(sdq_peer_t2),
      sdq_emotion_t2 + sdq_peer_t2,
      NA_real_
    ),
    
    sdq_internal_t5 = dplyr::if_else(
      !is.na(sdq_emotion_t5) &
        !is.na(sdq_peer_t5),
      sdq_emotion_t5 + sdq_peer_t5,
      NA_real_
    ),
    
    # Externalisierende Probleme:
    # Verhaltensprobleme + Hyperaktivität
    sdq_external_t2 = dplyr::if_else(
      !is.na(sdq_conduct_t2) &
        !is.na(sdq_hyper_t2),
      sdq_conduct_t2 + sdq_hyper_t2,
      NA_real_
    ),
    
    sdq_external_t5 = dplyr::if_else(
      !is.na(sdq_conduct_t5) &
        !is.na(sdq_hyper_t5),
      sdq_conduct_t5 + sdq_hyper_t5,
      NA_real_
    ),
    
    # SDQ-Gesamtproblemwert:
    # Emotion + Peer + Conduct + Hyperaktivität
    sdq_total_t2 = dplyr::if_else(
      !is.na(sdq_emotion_t2) &
        !is.na(sdq_peer_t2) &
        !is.na(sdq_conduct_t2) &
        !is.na(sdq_hyper_t2),
      sdq_emotion_t2 +
        sdq_peer_t2 +
        sdq_conduct_t2 +
        sdq_hyper_t2,
      NA_real_
    ),
    
    sdq_total_t5 = dplyr::if_else(
      !is.na(sdq_emotion_t5) &
        !is.na(sdq_peer_t5) &
        !is.na(sdq_conduct_t5) &
        !is.na(sdq_hyper_t5),
      sdq_emotion_t5 +
        sdq_peer_t5 +
        sdq_conduct_t5 +
        sdq_hyper_t5,
      NA_real_
    )
  )

predictor_blocks <- list(
  
  demographics = c(
    "sex",
    "age_t2",
    "maternal_education"
  ),
  
  baseline_internal = c(
    "sdq_internal_t2"
  ),
  
  baseline_external = c(
    "sdq_external_t2"
  ),
  
  baseline_total = "sdq_total_t2",
  
  maltreatment = c(
    "mt_subtypes_t1",
    "mt_frequency_t1",
    "mt_severity_t1"
  ),
  
  genetic_pcs = c(
    "genetic_pc1",
    "genetic_pc2",
    "genetic_pc3",
    "genetic_pc4"
  ),
  
  prs = "prs_mdd",
  
  hair_cortisol = "hcc_t2_z"
)

##### DEFINE FIRST MODEL SCENARIOS #####

model_scenarios_internal <- list(
  
  M0_demographics = predictor_blocks$demographics,
  
  M1_baseline_sdq = c(
    predictor_blocks$demographics,
    predictor_blocks$baseline_internal
  ),
  
  M2_maltreatment = c(
    predictor_blocks$demographics,
    predictor_blocks$baseline_internal,
    predictor_blocks$maltreatment
  )
)


model_scenarios_external <- list(
  
  M0_demographics = predictor_blocks$demographics,
  
  M1_baseline_sdq = c(
    predictor_blocks$demographics,
    predictor_blocks$baseline_external
  ),
  
  M2_maltreatment = c(
    predictor_blocks$demographics,
    predictor_blocks$baseline_external,
    predictor_blocks$maltreatment
  )
)

model_scenarios_total <- list(
  
  M0_demographics = predictor_blocks$demographics,
  
  M1_baseline_sdq = c(
    predictor_blocks$demographics,
    predictor_blocks$baseline_total
  ),
  
  M2_maltreatment = c(
    predictor_blocks$demographics,
    predictor_blocks$baseline_total,
    predictor_blocks$maltreatment
  )
)

##### CHECK OUTCOME AVAILABILITY #####

outcome_overview <- amis_ml |>
  dplyr::summarise(
    n_total = dplyr::n(),
    
    n_internal_t5 = sum(
      !is.na(sdq_internal_t5)
    ),
    
    n_external_t5 = sum(
      !is.na(sdq_external_t5)
    ),
    
    percent_missing_internal_t5 =
      mean(is.na(sdq_internal_t5)) * 100,
    
    percent_missing_external_t5 =
      mean(is.na(sdq_external_t5)) * 100
  )

print(outcome_overview)


##### DESCRIBE SDQ SCORES #####

sdq_score_overview <- amis_ml |>
  dplyr::summarise(
    dplyr::across(
      c(
        sdq_internal_t2,
        sdq_internal_t5,
        sdq_external_t2,
        sdq_external_t5
      ),
      list(
        n = \(x) sum(!is.na(x)),
        mean = \(x) mean(x, na.rm = TRUE),
        sd = \(x) stats::sd(x, na.rm = TRUE),
        min = \(x) min(x, na.rm = TRUE),
        max = \(x) max(x, na.rm = TRUE)
      )
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c(
      "score",
      ".value"
    ),
    names_pattern = "(.*)_(n|mean|sd|min|max)$"
  )

print(
  sdq_score_overview,
  n = Inf
)


##### SHOW MODEL DEFINITIONS #####

cat("\n\n===== INTERNALIZING MODELS =====\n")
print(model_scenarios_internal)

cat("\n\n===== EXTERNALIZING MODELS =====\n")
print(model_scenarios_external)


##### FINAL MESSAGE #####

message(
  "Analysis dataset successfully created as: amis_ml"
)


