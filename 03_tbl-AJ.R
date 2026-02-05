############################################################################### #
# Aim ----
#| produce tables (Mondays only)
#| last 10 Monday sampling dates up to date_reporting
#| save tables for sars, rsv and flu (for report)
############################################################################### #

# Load packages ----
pkgs <- c("dplyr", "lubridate", "readr", "flextable", "officer")
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, library, character.only = TRUE))

# --------------------------------------------------------------------------- #
# Data source logic ----
# Use fake file until 01_data_prep.R is ready.
# When 01_data_prep.R is ready, it should define `date_reporting`
# and export ./data/Belgium_export-nation.csv
# --------------------------------------------------------------------------- #

# Optionally source data prep (uncomment when ready)
# source("./01_data_prep.R")

use_real <- file.exists("./data/Belgium_export-nation.csv")

if (use_real) {
  df_nation <- read.table("./data/Belgium_export-nation.csv",
                          sep = ";", dec = ".", header = TRUE)
  df_nation$date <- as.Date(df_nation$date)
  
  if (!exists("date_reporting")) {
    # safety fallback (should come from 01_data_prep.R)
    date_reporting <- max(df_nation$date, na.rm = TRUE)
    message("NOTE: date_reporting not found. Using max(date) = ", date_reporting)
  }
} else {
  df_nation <- readr::read_delim("./Belgium_export-nation-sc_flu_rsv-fake.csv",
                                 delim = ";", col_types = cols())
  df_nation <- df_nation %>% mutate(date = as.Date(date))
  
  # fallback until 01_data_prep.R is ready
  if (!exists("date_reporting")) {
    date_reporting <- max(df_nation$date, na.rm = TRUE)
    message("NOTE: Using fake file. date_reporting fallback = ", date_reporting)
  }
}

# --------------------------------------------------------------------------- #
# Helper: build + save table for one virus ----
# NOTE: current nation/fake data has no virus column, so we reuse same data
# for sars/rsv/flu until real separated outputs exist.
# --------------------------------------------------------------------------- #

make_save_tbl <- function(df, virus_name) {
  
  out <- df %>%
    filter(date <= date_reporting) %>%
    # Mondays only (week_start=1 => Monday is 1)
    filter(lubridate::wday(date, week_start = 1) == 1) %>%
    select(siteName, date, value_pmmv) %>%
    arrange(desc(date)) %>%
    slice_head(n = 10) %>%
    arrange(date) %>%                    # nicer for report
    mutate(date = format(date, "%Y-%m-%d"))
  
  ft <- flextable(out) %>%
    set_header_labels(
      siteName = "Site",
      date = "Date (Monday)",
      value_pmmv = "National viral ratio (value/PMMV)"
    ) %>%
    fontsize(part = "body", size = 10) %>%
    fontsize(part = "header", size = 10) %>%
    autofit() %>%
    theme_vanilla()
  
  dir.create("./tables", showWarnings = FALSE)
  saveRDS(ft, file = sprintf("./tables/tbl_nation_%s.rds", virus_name))
  flextable::save_as_docx(ft, path = sprintf("./tables/tbl_nation_%s.docx", virus_name))
  
  ft
}

# Build and save tables ----
tbl_sars <- make_save_tbl(df_nation, "sars")
tbl_rsv  <- make_save_tbl(df_nation, "rsv")
tbl_flu  <- make_save_tbl(df_nation, "flu")

# Print one (optional, useful in Quarto)
tbl_sars

cat("- Success : tables saved in ./tables (sars, rsv, flu)\n")
