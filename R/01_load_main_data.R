##### LOAD SETUP #####

source(here::here("R", "00_setup.R"))


##### DATA PATH #####

main_data_file <- paste0(
  "C:/Users/keil/seadrive_root/Jan Keil/",
  "Meine Bibliotheken/MAIN OUTCOME/",
  "02_data/02_data_Prep/",
  "AMIS_merged_analysis_dataset_with_M18_classes_SES_mo.xlsx"
)

if (!file.exists(main_data_file)) {
  stop(
    "Main outcome dataset not found:\n",
    main_data_file
  )
}


##### IMPORT MAIN DATA #####

# Importiert das erste Tabellenblatt der Excel-Datei.
# clean_names() vereinheitlicht die Variablennamen:
# Leerzeichen und Sonderzeichen werden beispielsweise durch "_" ersetzt.
amis_ml_raw <- readxl::read_excel(
  path = main_data_file,
  sheet = 1
) |>
  janitor::clean_names()


##### BASIC DATA CHECKS #####

cat("\nMain outcome dataset successfully imported.\n")
cat("Rows:", nrow(amis_ml_raw), "\n")
cat("Columns:", ncol(amis_ml_raw), "\n")


##### CHECK VARIABLE NAMES #####

# Prüft, ob durch clean_names() doppelte Variablennamen
# entstanden sind.
duplicated_variable_names <- names(amis_ml_raw)[
  duplicated(names(amis_ml_raw))
]

if (length(duplicated_variable_names) > 0) {
  
  warning(
    "Duplicated variable names found: ",
    paste(
      duplicated_variable_names,
      collapse = ", "
    )
  )
  
} else {
  
  message("No duplicated variable names found.")
  
}


##### DISPLAY DATA STRUCTURE #####

dplyr::glimpse(amis_ml_raw)


##### FINISH #####

message("Main outcome dataset is available as: amis_ml_raw")
