###############################################################################
# Aim ----
# Produce national tables for Quarto
# load Belgium national data
# keep last 10 MONDAY sampling dates
# last date = date_reporting
# nice headers, units, digits
# test different flextable themes
###############################################################################

# 0) Load packages ----
pkgs <- c("dplyr", "lubridate", "flextable", "readr", "stringr")
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, library, character.only = TRUE))

# 1) Load data from required locations ----
path1 <- ".data/Belgium_export-nation.csv"
path2 <- "Belgium_export-nation.csv"   # fallback

if (file.exists(path1)) {
  df_nation2 <- readr::read_delim(path1, delim = ";", show_col_types = FALSE)
} else if (file.exists(path2)) {
  df_nation2 <- readr::read_delim(path2, delim = ";", show_col_types = FALSE)
} else {
  stop("File not found. Expected .data/Belgium_export-nation.csv or Belgium_export-nation.csv")
}

# 2) Read date_reporting safely from 01_data_prep.R ----
if (!file.exists("01_data_prep.R")) {
  stop("File 01_data_prep.R not found")
}

lines <- readLines("01_data_prep.R", warn = FALSE)

line_idx <- grep("^\\s*date_reporting\\s*<-", lines)
if (length(line_idx) == 0) stop("No line like 'date_reporting <- ...' found in 01_data_prep.R")

dr_line <- lines[line_idx[1]]
dr_value <- stringr::str_extract(dr_line, "\\d{4}-\\d{2}-\\d{2}")

if (is.na(dr_value)) stop("date_reporting line found but wrong date format (YYYY-MM-DD).")

date_reporting <- as.Date(dr_value)

# 3) Prepare data + keep only Mondays + up to date_reporting ----
needed_cols <- c("date", "value_pmmv")
missing_cols <- setdiff(needed_cols, names(df_nation2))
if (length(missing_cols) > 0) stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))

df_tbl <- df_nation2 %>%
  mutate(
    date = as.character(`date`),  # force character
    date = dmy(date),             # convert to Date (dd/mm/yyyy or dd-mm-yyyy)
    value_pmmv = as.numeric(value_pmmv)
  ) %>%
  filter(!is.na(date)) %>%
  filter(lubridate::wday(date, week_start = 1) == 1) %>%  # only Mondays
  filter(date <= date_reporting) %>%
  arrange(desc(date)) %>%
  slice_head(n = 10) %>%
  arrange(date) %>%
  transmute(
    `Sampling date (Monday)` = format(date, "%d %b %Y"),
    `National viral ratio (PMMoV-normalized)` = value_pmmv
  )

# 4) Create flextable + formatting ----
tbl_nation_Antea <- flextable(df_tbl) %>%
  colformat_num(j = "National viral ratio (PMMoV-normalized)", digits = 4) %>%
  align(align = "center", part = "all") %>%
  fontsize(part = "body", size = 10) %>%
  fontsize(part = "header", size = 10) %>%
  flextable::autofit()

# Test themes ----
tbl_nation_vanilla <- tbl_nation_Antea %>% theme_vanilla()
zebra_tbl_nation_Antea <- tbl_nation_Antea %>% theme_zebra()  # chosen theme

tbl_nation_Antea <- zebra_tbl_nation_Antea

# 5) Save for Quarto ----
if (!dir.exists(".data")) dir.create(".data")
saveRDS(tbl_nation_Antea, file = ".data/tbl_nation_10_mondays_Antea.rds")

# 6) Print & message ----
tbl_nation_Antea

cat(paste0(
  "- Success: table saved -> .data/tbl_nation_10_mondays_Antea.rds (date_reporting = ",
  as.character(date_reporting), ")\n"
))
