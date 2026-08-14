##### LOAD SETUP AND MAIN DATA #####

source(
  here::here(
    "R",
    "03_prepare_analysis_data.R"
  )
)


##### CBCL DATA DIRECTORIES #####

cbcl_t2_directory <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/",
  "Meine Bibliotheken/MAIN OUTCOME/",
  "02_data/00_raw_final/AMIS 1/data"
)

cbcl_t5_directory <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/",
  "Meine Bibliotheken/MAIN OUTCOME/",
  "02_data/00_raw_final/AMIS 2/data"
)


##### CHECK DIRECTORIES #####

if (!dir.exists(cbcl_t2_directory)) {
  stop(
    "AMIS 1 data directory not found:\n",
    cbcl_t2_directory
  )
}

if (!dir.exists(cbcl_t5_directory)) {
  stop(
    "AMIS 2 data directory not found:\n",
    cbcl_t5_directory
  )
}


##### FUNCTION: FIND ONE FILE #####

find_table_file <- function(
    root,
    file_pattern
) {
  
  matching_files <- list.files(
    path = root,
    pattern = file_pattern,
    recursive = FALSE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # Temporäre und Sicherungsdateien ausschließen
  matching_files <- matching_files[
    !stringr::str_detect(
      basename(matching_files),
      "^~\\$|backup|copy|kopie"
    )
  ]
  
  if (length(matching_files) == 0) {
    stop(
      "No file found for pattern ",
      file_pattern,
      " in:\n",
      root
    )
  }
  
  if (length(matching_files) > 1) {
    stop(
      "More than one file found for pattern ",
      file_pattern,
      ":\n",
      paste(
        matching_files,
        collapse = "\n"
      )
    )
  }
  
  matching_files[[1]]
}


##### LOCATE CBCL FILES #####

cbcl_t2_file <- find_table_file(
  root = cbcl_t2_directory,
  file_pattern = "^PV0880_T00614_NODUP"
)

cbcl_t5_file <- find_table_file(
  root = cbcl_t5_directory,
  file_pattern = "^PV0880_T01332"
)


##### PRINT FILE PATHS #####

cat(
  "\nCBCL T2 file:\n",
  cbcl_t2_file,
  "\n"
)

cat(
  "\nCBCL T5 file:\n",
  cbcl_t5_file,
  "\n"
)


##### IMPORT CBCL EXCEL FILES #####

cbcl_t2_raw <- readxl::read_excel(
  path = cbcl_t2_file,
  sheet = 1
) |>
  janitor::clean_names()

cbcl_t5_raw <- readxl::read_excel(
  path = cbcl_t5_file,
  sheet = 1
) |>
  janitor::clean_names()


##### BASIC DATA CHECKS #####

cat(
  "\nCBCL T2:",
  nrow(cbcl_t2_raw),
  "rows and",
  ncol(cbcl_t2_raw),
  "columns\n"
)

cat(
  "CBCL T5:",
  nrow(cbcl_t5_raw),
  "rows and",
  ncol(cbcl_t5_raw),
  "columns\n"
)


##### CHECK SIC VARIABLE NAMES #####

print(
  names(cbcl_t2_raw)[
    stringr::str_detect(
      names(cbcl_t2_raw),
      stringr::regex(
        "sic",
        ignore_case = TRUE
      )
    )
  ]
)

print(
  names(cbcl_t5_raw)[
    stringr::str_detect(
      names(cbcl_t5_raw),
      stringr::regex(
        "sic",
        ignore_case = TRUE
      )
    )
  ]
)

print(
  names(amis_ml_raw)[
    stringr::str_detect(
      names(amis_ml_raw),
      stringr::regex(
        "sic",
        ignore_case = TRUE
      )
    )
  ]
)

##### CHECK MISSING SIC VALUES #####

cat(
  "\nMissing SIC values in CBCL T2:",
  sum(is.na(cbcl_t2_raw$sic)),
  "\n"
)

cat(
  "Missing SIC values in CBCL T5:",
  sum(is.na(cbcl_t5_raw$sic)),
  "\n"
)


##### CHECK DUPLICATED SIC VALUES #####

duplicated_sic_t2 <- cbcl_t2_raw |>
  dplyr::filter(
    !is.na(sic)
  ) |>
  dplyr::count(
    sic,
    name = "n"
  ) |>
  dplyr::filter(
    n > 1
  )

duplicated_sic_t5 <- cbcl_t5_raw |>
  dplyr::filter(
    !is.na(sic)
  ) |>
  dplyr::count(
    sic,
    name = "n"
  ) |>
  dplyr::filter(
    n > 1
  )

cat(
  "Duplicated SIC values in CBCL T2:",
  nrow(duplicated_sic_t2),
  "\n"
)

cat(
  "Duplicated SIC values in CBCL T5:",
  nrow(duplicated_sic_t5),
  "\n"
)

if (nrow(duplicated_sic_t2) > 0) {
  print(
    duplicated_sic_t2,
    n = Inf
  )
}

if (nrow(duplicated_sic_t5) > 0) {
  print(
    duplicated_sic_t5,
    n = Inf
  )
}

##### FUNCTION: CHECK AND CLEAN CBCL ITEMS #####

clean_cbcl_items <- function(
    data,
    prefix,
    timepoint
) {
  
  # Erkennt ausschließlich CBCL-Itemvariablen wie:
  # cbcl_e_t2_1, cbcl_e_t2_56a, cbcl_b_t5_112
  item_variables <- names(data)[
    stringr::str_detect(
      names(data),
      stringr::regex(
        paste0(
          "^",
          prefix,
          "[0-9]+[a-z]*$"
        ),
        ignore_case = TRUE
      )
    )
  ]
  
  if (length(item_variables) == 0) {
    stop(
      "No CBCL item variables found for ",
      timepoint,
      "."
    )
  }
  
  cat(
    "\nNumber of identified CBCL items in ",
    timepoint,
    ": ",
    length(item_variables),
    "\n",
    sep = ""
  )
  
  
  ##### CONVERT ITEMS TO NUMERIC #####
  
  data <- data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(item_variables),
        \(x) suppressWarnings(
          as.numeric(x)
        )
      )
    )
  
  
  ##### IDENTIFY INVALID VALUES #####
  
  invalid_values <- data |>
    dplyr::select(
      dplyr::all_of(item_variables)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::filter(
      !is.na(value),
      !value %in% c(0, 1, 2)
    ) |>
    dplyr::count(
      variable,
      value,
      name = "n"
    ) |>
    dplyr::arrange(
      variable,
      value
    )
  
  cat(
    "\nInvalid CBCL item values in ",
    timepoint,
    ":\n",
    sep = ""
  )
  
  if (nrow(invalid_values) == 0) {
    
    message(
      "No invalid item values found."
    )
    
  } else {
    
    print(
      invalid_values,
      n = Inf
    )
  }
  
  
  ##### RECODE INVALID VALUES TO NA #####
  
  data <- data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(item_variables),
        \(x) dplyr::if_else(
          is.na(x) |
            x %in% c(0, 1, 2),
          x,
          NA_real_
        )
      )
    )
  
  list(
    data = data,
    invalid_values = invalid_values,
    item_variables = item_variables
  )
}

##### CLEAN T2 ITEM VALUES #####

cbcl_t2_cleaning <- clean_cbcl_items(
  data = cbcl_t2_raw,
  prefix = "cbcl_e_t2_",
  timepoint = "T2"
)

cbcl_t2_raw <- cbcl_t2_cleaning$data

cbcl_t2_invalid_values <-
  cbcl_t2_cleaning$invalid_values


##### CLEAN T5 ITEM VALUES #####

cbcl_t5_cleaning <- clean_cbcl_items(
  data = cbcl_t5_raw,
  prefix = "cbcl_b_t5_",
  timepoint = "T5"
)

cbcl_t5_raw <- cbcl_t5_cleaning$data

cbcl_t5_invalid_values <-
  cbcl_t5_cleaning$invalid_values



##### FUNCTION: PRORATED SUM SCORE #####

# Entspricht beispielsweise:
# MEAN.8(item1, ..., item9) * 9
#
# Der Skalenwert wird hochgerechnet, wenn mindestens
# "min_valid" Items vorhanden sind.

prorated_sum <- function(
    data,
    variables,
    min_valid
) {
  
  missing_variables <- setdiff(
    variables,
    names(data)
  )
  
  if (length(missing_variables) > 0) {
    stop(
      "The following CBCL items are missing:\n",
      paste(
        missing_variables,
        collapse = "\n"
      )
    )
  }
  
  item_matrix <- data |>
    dplyr::select(
      dplyr::all_of(variables)
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.numeric
      )
    ) |>
    as.matrix()
  
  n_available <- rowSums(
    !is.na(item_matrix)
  )
  
  score <- rowMeans(
    item_matrix,
    na.rm = TRUE
  ) * length(variables)
  
  score[
    n_available < min_valid
  ] <- NA_real_
  
  score[
    n_available == 0
  ] <- NA_real_
  
  score
}


##### FUNCTION: CREATE CBCL SCALES #####

create_cbcl_scales <- function(
    data,
    prefix
) {
  
  # Erzeugt die tatsächlichen Variablennamen
  # aus Präfix und CBCL-Itemnummer.
  item_names <- function(items) {
    paste0(
      prefix,
      items
    )
  }
  
  
  ##### SOCIAL WITHDRAWAL #####
  
  social_withdrawal_items <- item_names(
    c(
      "42",
      "65",
      "69",
      "75",
      "80",
      "88",
      "102",
      "103",
      "111"
    )
  )
  
  
  ##### SOMATIC COMPLAINTS #####
  
  somatic_complaints_items <- item_names(
    c(
      "51",
      "54",
      "56a",
      "56b",
      "56c",
      "56d",
      "56e",
      "56f",
      "56g"
    )
  )
  
  
  ##### ANXIOUS/DEPRESSED #####
  
  anxious_depressed_items <- item_names(
    c(
      "12",
      "14",
      "31",
      "32",
      "33",
      "34",
      "35",
      "45",
      "50",
      "52",
      "71",
      "89",
      "103",
      "112"
    )
  )
  
  
  ##### DELINQUENT BEHAVIOR #####
  
  delinquent_behavior_items <- item_names(
    c(
      "26",
      "39",
      "43",
      "63",
      "67",
      "72",
      "81",
      "82",
      "90",
      "96",
      "101",
      "105",
      "106"
    )
  )
  
  
  ##### AGGRESSIVE BEHAVIOR #####
  
  aggressive_behavior_items <- item_names(
    c(
      "3",
      "7",
      "16",
      "19",
      "20",
      "21",
      "22",
      "23",
      "27",
      "37",
      "57",
      "68",
      "74",
      "86",
      "87",
      "93",
      "94",
      "95",
      "97",
      "104"
    )
  )
  
  
  ##### TOTAL-PROBLEMS ITEMS #####
  
  # Entspricht der Itemliste des Gesamtauffälligkeitswerts
  # in der vorliegenden SPSS-Syntax.
  total_problem_items <- item_names(
    c(
      "1",
      "3",
      as.character(5:55),
      "56a",
      "56b",
      "56c",
      "56d",
      "56e",
      "56f",
      "56g",
      "56h",
      as.character(57:112)
    )
  )
  
  
  ##### CREATE SYNDROME SCALES #####
  
  # 8 von 9 Items müssen vorhanden sein.
  cbcl_social_withdrawal <- prorated_sum(
    data = data,
    variables = social_withdrawal_items,
    min_valid = 8
  )
  
  # 8 von 9 Items müssen vorhanden sein.
  cbcl_somatic_complaints <- prorated_sum(
    data = data,
    variables = somatic_complaints_items,
    min_valid = 8
  )
  
  # 13 von 14 Items müssen vorhanden sein.
  cbcl_anxious_depressed <- prorated_sum(
    data = data,
    variables = anxious_depressed_items,
    min_valid = 13
  )
  
  # 12 von 13 Items müssen vorhanden sein.
  cbcl_delinquent_behavior <- prorated_sum(
    data = data,
    variables = delinquent_behavior_items,
    min_valid = 12
  )
  
  # 19 von 20 Items müssen vorhanden sein.
  cbcl_aggressive_behavior <- prorated_sum(
    data = data,
    variables = aggressive_behavior_items,
    min_valid = 19
  )
  
  
  ##### CREATE INTERNALIZING SCORE #####
  
  # Item 103 kommt sowohl in sozialem Rückzug als auch
  # in ängstlich/depressiv vor und wird deshalb einmal abgezogen.
  item_103 <- as.numeric(
    data[[paste0(prefix, "103")]]
  )
  
  cbcl_internal <- (
    cbcl_social_withdrawal +
      cbcl_somatic_complaints +
      cbcl_anxious_depressed -
      item_103
  )
  
  
  ##### CREATE EXTERNALIZING SCORE #####
  
  cbcl_external <- (
    cbcl_delinquent_behavior +
      cbcl_aggressive_behavior
  )
  
  
  ##### CREATE TOTAL-PROBLEMS SCORE #####
  
  # Die SPSS-Syntax verwendet hier SUM() ohne explizite
  # Mindestanzahl gültiger Items.
  #
  # Für die ML-Analyse verlangen wir mindestens 90 %
  # vorhandene Items und rechnen den Wert anschließend
  # auf die vollständige Itemzahl hoch.
  
  minimum_total_items <- ceiling(
    length(total_problem_items) * 0.90
  )
  
  cbcl_total <- prorated_sum(
    data = data,
    variables = total_problem_items,
    min_valid = minimum_total_items
  )
  
  
  ##### RETURN SCORES #####
  
  tibble::tibble(
    sic = data$sic,
    
    cbcl_social_withdrawal =
      cbcl_social_withdrawal,
    
    cbcl_somatic_complaints =
      cbcl_somatic_complaints,
    
    cbcl_anxious_depressed =
      cbcl_anxious_depressed,
    
    cbcl_delinquent_behavior =
      cbcl_delinquent_behavior,
    
    cbcl_aggressive_behavior =
      cbcl_aggressive_behavior,
    
    cbcl_internal =
      cbcl_internal,
    
    cbcl_external =
      cbcl_external,
    
    cbcl_total =
      cbcl_total
  )
}


##### CREATE T2 SCORES #####

cbcl_t2_scored <- create_cbcl_scales(
  data = cbcl_t2_raw,
  prefix = "cbcl_e_t2_"
) |>
  dplyr::rename(
    cbcl_social_withdrawal_t2 =
      cbcl_social_withdrawal,
    
    cbcl_somatic_complaints_t2 =
      cbcl_somatic_complaints,
    
    cbcl_anxious_depressed_t2 =
      cbcl_anxious_depressed,
    
    cbcl_delinquent_behavior_t2 =
      cbcl_delinquent_behavior,
    
    cbcl_aggressive_behavior_t2 =
      cbcl_aggressive_behavior,
    
    cbcl_internal_t2 =
      cbcl_internal,
    
    cbcl_external_t2 =
      cbcl_external,
    
    cbcl_total_t2 =
      cbcl_total
  )


##### CREATE T5 SCORES #####

cbcl_t5_scored <- create_cbcl_scales(
  data = cbcl_t5_raw,
  prefix = "cbcl_b_t5_"
) |>
  dplyr::rename(
    cbcl_social_withdrawal_t5 =
      cbcl_social_withdrawal,
    
    cbcl_somatic_complaints_t5 =
      cbcl_somatic_complaints,
    
    cbcl_anxious_depressed_t5 =
      cbcl_anxious_depressed,
    
    cbcl_delinquent_behavior_t5 =
      cbcl_delinquent_behavior,
    
    cbcl_aggressive_behavior_t5 =
      cbcl_aggressive_behavior,
    
    cbcl_internal_t5 =
      cbcl_internal,
    
    cbcl_external_t5 =
      cbcl_external,
    
    cbcl_total_t5 =
      cbcl_total
  )


##### CHECK T2 DISTRIBUTIONS #####

cbcl_t2_overview <- cbcl_t2_scored |>
  dplyr::summarise(
    dplyr::across(
      c(
        cbcl_internal_t2,
        cbcl_external_t2,
        cbcl_total_t2
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
      "scale",
      ".value"
    ),
    names_pattern = "(.*)_(n|mean|sd|min|max)$"
  )

print(
  cbcl_t2_overview,
  n = Inf
)


##### CHECK T5 DISTRIBUTIONS #####

cbcl_t5_overview <- cbcl_t5_scored |>
  dplyr::summarise(
    dplyr::across(
      c(
        cbcl_internal_t5,
        cbcl_external_t5,
        cbcl_total_t5
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
      "scale",
      ".value"
    ),
    names_pattern = "(.*)_(n|mean|sd|min|max)$"
  )

print(
  cbcl_t5_overview,
  n = Inf
)


##### FINAL MESSAGE #####

message(
  "CBCL T2 and T5 scores were successfully created."
)

##### VALIDATE THEORETICAL SCORE RANGES #####

validate_cbcl_ranges <- function(
    data,
    timepoint
) {
  
  range_problems <- data |>
    dplyr::filter(
      (!is.na(.data[[paste0("cbcl_internal_", timepoint)]]) &
         (
           .data[[paste0("cbcl_internal_", timepoint)]] < 0 |
             .data[[paste0("cbcl_internal_", timepoint)]] > 62
         )) |
        (!is.na(.data[[paste0("cbcl_external_", timepoint)]]) &
           (
             .data[[paste0("cbcl_external_", timepoint)]] < 0 |
               .data[[paste0("cbcl_external_", timepoint)]] > 66
           )) |
        (!is.na(.data[[paste0("cbcl_total_", timepoint)]]) &
           (
             .data[[paste0("cbcl_total_", timepoint)]] < 0 |
               .data[[paste0("cbcl_total_", timepoint)]] > 234
           ))
    )
  
  if (nrow(range_problems) > 0) {
    
    stop(
      "Implausible CBCL scale scores found at ",
      timepoint,
      "."
    )
    
  } else {
    
    message(
      "All CBCL scores at ",
      timepoint,
      " are within their theoretical ranges."
    )
  }
}


validate_cbcl_ranges(
  data = cbcl_t2_scored,
  timepoint = "t2"
)

validate_cbcl_ranges(
  data = cbcl_t5_scored,
  timepoint = "t5"
)

##### SUMMARIZE INVALID ITEM VALUES #####

cat(
  "\nTotal invalid item values recoded to NA at T2:",
  sum(cbcl_t2_invalid_values$n),
  "\n"
)

cat(
  "Total invalid item values recoded to NA at T5:",
  sum(cbcl_t5_invalid_values$n),
  "\n"
)

