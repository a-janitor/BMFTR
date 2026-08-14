##### 1. LOAD DATA #####

# Lädt Pakete, Pfade und den Main-Outcome-Datensatz.
source(here::here("R", "01_load_main_data.R"))


##### 2. GENERAL DATA CHECK #####

cat("\nNumber of participants:", nrow(amis_ml_raw), "\n")
cat("Number of variables:", ncol(amis_ml_raw), "\n")


##### CREATE VARIABLE INVENTORY #####

variable_inventory <- tibble::tibble(
  variable = names(amis_ml_raw),
  
  type = purrr::map_chr(
    amis_ml_raw,
    \(x) class(x)[1]
  ),
  
  n_available = purrr::map_int(
    amis_ml_raw,
    \(x) sum(!is.na(x))
  ),
  
  n_missing = purrr::map_int(
    amis_ml_raw,
    \(x) sum(is.na(x))
  ),
  
  percent_missing = purrr::map_dbl(
    amis_ml_raw,
    \(x) mean(is.na(x)) * 100
  ),
  
  n_unique = purrr::map_int(
    amis_ml_raw,
    \(x) dplyr::n_distinct(
      x,
      na.rm = TRUE
    )
  )
)


##### SEARCH FUNCTION #####

find_variables <- function(pattern) {
  
  variable_inventory |>
    dplyr::filter(
      stringr::str_detect(
        variable,
        stringr::regex(
          pattern,
          ignore_case = TRUE
        )
      )
    ) |>
    dplyr::arrange(
      percent_missing,
      variable
    )
}


##### IDENTIFIER #####

id_candidates <- find_variables(
  "id|subject|participant|proband|person"
)


##### DEMOGRAPHICS #####

demographic_candidates <- find_variables(
  paste(
    c(
      "age",
      "alter",
      "sex",
      "gender",
      "geschlecht",
      "education",
      "bildung",
      "school",
      "ses"
    ),
    collapse = "|"
  )
)


##### SDQ #####

sdq_candidates <- find_variables(
  paste(
    c(
      "sdq",
      "external",
      "internal",
      "emotion",
      "conduct",
      "hyper",
      "peer",
      "prosocial"
    ),
    collapse = "|"
  )
)


##### MALTREATMENT #####

maltreatment_candidates <- find_variables(
  paste(
    c(
      "micm",
      "cicm",
      "mcs",
      "maltreat",
      "burden",
      "class",
      "severity",
      "frequency",
      "subtype"
    ),
    collapse = "|"
  )
)


##### POLYGENIC RISK SCORES #####

prs_candidates <- find_variables(
  "prs|polygenic|pgs|mdd|pca|pc"
)


##### HAIR CORTISOL #####

hcc_candidates <- find_variables(
  "hcc|hair|haar|cortisol|cort"
)


##### COMBINE ALL CANDIDATES #####

candidate_variables <- dplyr::bind_rows(
  id_candidates |>
    dplyr::mutate(block = "identifier"),
  
  demographic_candidates |>
    dplyr::mutate(block = "demographics"),
  
  sdq_candidates |>
    dplyr::mutate(block = "sdq"),
  
  maltreatment_candidates |>
    dplyr::mutate(block = "maltreatment"),
  
  prs_candidates |>
    dplyr::mutate(block = "prs"),
  
  hcc_candidates |>
    dplyr::mutate(block = "hair_cortisol")
) |>
  dplyr::distinct(
    block,
    variable,
    .keep_all = TRUE
  ) |>
  dplyr::select(
    block,
    variable,
    type,
    n_available,
    n_missing,
    percent_missing,
    n_unique
  ) |>
  dplyr::arrange(
    block,
    percent_missing,
    variable
  )


##### PRINT RESULTS #####

cat("\n\n===== ID CANDIDATES =====\n")
print(id_candidates, n = Inf)

cat("\n\n===== DEMOGRAPHIC CANDIDATES =====\n")
print(demographic_candidates, n = Inf)

cat("\n\n===== SDQ CANDIDATES =====\n")
print(sdq_candidates, n = Inf)

cat("\n\n===== MALTREATMENT CANDIDATES =====\n")
print(maltreatment_candidates, n = Inf)

cat("\n\n===== PRS CANDIDATES =====\n")
print(prs_candidates, n = Inf)

cat("\n\n===== HAIR CORTISOL CANDIDATES =====\n")
print(hcc_candidates, n = Inf)
