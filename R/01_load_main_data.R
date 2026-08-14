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

