###############################################################################
# Aim ----
#Produce national tables for Quarto
#load Belgium national data
#keep last 10 MONDAY sampling dates
#last date = date_reporting
#nice headers, units, digits
#test different flextable themes
###############################################################################

# Load packages ----
pkgs <- c("dplyr", "lubridate", "flextable", "readr")
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, library, character.only = TRUE))

# Load data ----
path_main <- "./data/Belgium_export-nation.csv"
df_nation <- readr::read_csv(path_main) 

#pretvorara datum i dodaje weekday
df_nation <- df_nation %>%
  mutate(
    date = as.character(date),  # prvo u character
    date = dmy(date),            # onda u Date
    weekday = wday(date, label = TRUE, week_start = 1)
  )

# filtrira samo mondays
df_nation_mon <- df_nation %>%
  filter(weekday == "Mon")

# keep last reporting date ----
date_reporting <- max(df_nation_mon$date, na.rm = TRUE)

# select last 10 dates ----
tbl_data <- df_nation_mon %>%
  filter(date <= date_reporting) %>%
  arrange(desc(date)) %>%
  slice_head(n = 10) %>%
  arrange(date)
library(dplyr)
library(flextable)

# Create flextable ----
tbl_nation <- tbl_data %>%
  transmute(
    sampling_date = format(date, "%d %b %Y"),           # mala slova, sigurno ime
    national_viral_load = round(value_pmmv, 2)         # sigurno ime
  ) %>%
  flextable() %>%
  set_header_labels(
    sampling_date = "Sampling date",
    national_viral_load = "National viral load (PMMoV-normalised)"
  ) %>%
  colformat_num(
    j = "national_viral_load",   # koristi stvarno ime kolone, ne prikazno
    digits = 2,
    big.mark = " ",
    decimal.mark = "."
  ) %>%
  flextable::autofit()

# Test themes (choose ONE for final use) ----
tbl_nation_theme_vanilla <- tbl_nation %>% theme_vanilla()
tbl_nation_theme_booktabs <- tbl_nation %>% theme_booktabs()
tbl_nation_theme_box <- tbl_nation %>% theme_box()

# FINAL table used by 04_quarto.R ----
tbl_nation_final <- tbl_nation_theme_vanilla

# Print table ----
tbl_nation_final

# Message ----
cat(
  "- Success: national table created\n",
  "- Last reporting date:", format(date_reporting, "%d %b %Y"), "\n"
)
