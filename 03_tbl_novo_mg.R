###############################################################################
# 03_tbl.R — National table (last 10 Monday sampling dates up to date_reporting)
###############################################################################

# Packages ----
pkgs <- c("dplyr", "lubridate", "flextable")
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

###############################################################################
# 1) Load data (preferred + fallback)  ----------------------------------------
###############################################################################
preferred_file <- file.path("data", "Belgium_export-nation.csv")
fallback_file  <- "Belgium_export-nation.csv"

input_file <- if (file.exists(preferred_file)) preferred_file else fallback_file
if (!file.exists(input_file)) {
  stop("Could not find input CSV. Expected ./data/Belgium_export-nation.csv (preferred) or ./Belgium_export-nation.csv (fallback).")
}

# IMPORTANT: your file is ';' separated
df_nation <- read.csv(input_file, sep = ";", header = TRUE, stringsAsFactors = FALSE)

###############################################################################
# 2) Ensure required columns + types ------------------------------------------
###############################################################################
needed <- c("date", "value_pmmv", "value_pmmv_avg14d_past")
missing <- setdiff(needed, names(df_nation))
if (length(missing) > 0) {
  stop(paste0(
    "Missing required column(s): ", paste(missing, collapse = ", "), "\n",
    "Columns found: ", paste(names(df_nation), collapse = ", ")
  ))
}

df_nation <- df_nation %>%
  mutate(
    date = as.Date(date),
    value_pmmv = as.numeric(value_pmmv),
    value_pmmv_avg14d_past = as.numeric(value_pmmv_avg14d_past)
  ) %>%
  filter(!is.na(date), !is.na(value_pmmv))

###############################################################################
# 3) date_reporting (prefer column if present, else max date) ------------------
###############################################################################
date_reporting <- if ("date_reporting" %in% names(df_nation)) {
  as.Date(df_nation$date_reporting[1])
} else {
  max(df_nation$date, na.rm = TRUE)
}

###############################################################################
# 4) Filter Mondays + last 10 dates up to date_reporting -----------------------
###############################################################################
df_tbl <- df_nation %>%
  filter(date <= date_reporting) %>%
  mutate(
    weekday = lubridate::wday(date, label = TRUE, week_start = 1)  # pon = 1
  ) %>%
  filter(weekday == "pon") %>%
  arrange(desc(date)) %>%
  slice_head(n = 10) %>%
  arrange(date)

###############################################################################
# 5) Build table with nice headers/units/digits --------------------------------
###############################################################################
tbl_base <- df_tbl %>%
  transmute(
    `Sampling date (ponedjeljak)` = date,
    `Viral ratio (PMMoV-normalised)` = value_pmmv,
    `14-day average (past)` = value_pmmv_avg14d_past
  ) %>%
  flextable() %>%
  colformat_num(j = c("Viral ratio (PMMoV-normalised)", "14-day average (past)"), digits = 2) %>%
  align(align = "left", part = "all") %>%
  fontsize(part = "body", size = 10) %>%
  fontsize(part = "header", size = 10) %>%
  autofit()

###############################################################################
# 6) Test different themes (preview in Viewer) --------------------------------
###############################################################################
tbl_vanilla <- tbl_base %>% theme_vanilla()
tbl_booktabs <- tbl_base %>% theme_booktabs()
tbl_box <- tbl_base %>% theme_box()
tbl_zebra <- tbl_base %>% theme_zebra()

# Preview themes
tbl_vanilla
tbl_booktabs
tbl_box
tbl_zebra

# Choose final theme here:
tbl_nation_final <- tbl_vanilla

###############################################################################
# 7) Save for 04_quarto.R ------------------------------------------------------
###############################################################################
if (!dir.exists("data")) dir.create("data", recursive = TRUE)

saveRDS(tbl_nation_final, file = "data/tbl_nation_last10_mondays.rds")

cat("- Success: table saved to data/tbl_nation_last10_mondays.rds\n")
cat("- date_reporting used:", as.character(date_reporting), "\n")
