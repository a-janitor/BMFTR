##### LOAD SETUP #####

source(here::here("R", "00_setup.R"))


##### FILE #####

instrument_file <- here::here(
  "data",
  "raw",
  "Instruments_AMIS.xlsx"
)

if (!file.exists(instrument_file)) {
  stop("Instruments_AMIS.xlsx was not found in data/raw.")
}


##### INSPECT WORKBOOK #####

sheet_names <- readxl::excel_sheets(instrument_file)

cat("\nAvailable sheets:\n")
print(sheet_names)


##### IMPORT ALL SHEETS #####

instrument_sheets <- sheet_names |>
  rlang::set_names() |>
  purrr::map(
    \(sheet) {
      readxl::read_excel(
        instrument_file,
        sheet = sheet
      ) |>
        janitor::clean_names()
    }
  )


##### SHOW STRUCTURE #####

sheet_overview <- purrr::imap_dfr(
  instrument_sheets,
  \(data, sheet) {
    tibble::tibble(
      sheet = sheet,
      rows = nrow(data),
      columns = ncol(data),
      variable_names = paste(names(data), collapse = ", ")
    )
  }
)

print(
  sheet_overview,
  n = Inf,
  width = Inf
)