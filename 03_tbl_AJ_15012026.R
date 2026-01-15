############################################################################### #
# Aim ----
#| produce tables
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
#| list of things to do...
############################################################################### #

# Load packages ----
# select packages
pkgs <- c("dplyr", "ggplot2", "flextable", "quarto")
# install packages
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

# load data

dir.create("data_example")
file.copy(
  from = "Belgium_export-nation.csv",
  to   = "data_example/Belgium_export-nation.csv",
  overwrite = TRUE
)
list.files("data_example")
df_nation <- read.table(file = "./data_example/Belgium_export-nation.csv", sep = ";", dec = ".", header = T)

# Save table ----
tbl_nation <- df_nation %>%
  select(siteName, date, value_pmmv) %>%
  arrange(desc(date)) %>% 
  slice_head(n = 10) %>%
  flextable() %>%
  fontsize(part = "body",size = 10) %>%
  fontsize(part = "header",size = 10) %>%
  autofit() %>% theme_vanilla()

tbl_nation

# display msg
cat("- Success : tables saved \n")

###############################################################################
# Solution part (added) ----
#| Use real data + create last-10-Mondays table up to date_reporting
###############################################################################

# Extra packages needed for this task (do NOT change the original pkgs block above)
pkgs_extra <- c("readr", "lubridate", "stringr")
install.packages(setdiff(pkgs_extra, rownames(installed.packages())))
invisible(lapply(pkgs_extra, FUN = library, character.only = TRUE))

# 1) Load data from required locations ----
path1 <- ".data/Belgium_export-nation.csv"
path2 <- "Belgium_export-nation.csv"   # fallback if you can't run 01_data_prep-solution.R

if (file.exists(path1)) {
  df_nation2 <- readr::read_delim(path1, delim = ";", show_col_types = FALSE)
} else if (file.exists(path2)) {
  df_nation2 <- readr::read_delim(path2, delim = ";", show_col_types = FALSE)
} else {
  stop("Ne mogu naći CSV. Očekujem: .data/Belgium_export-nation.csv ili Belgium_export-nation.csv")
}

# 2) Read date_reporting from 01_data_prep.R safely (without sourcing whole script) ----
# This avoids errors from executing other parts of 01_data_prep.R
if (!file.exists("01_data_prep.R")) {
  stop("Ne mogu naći 01_data_prep.R (treba za date_reporting).")
}

lines <- readLines("01_data_prep.R", warn = FALSE)

# Find a line like: date_reporting <- "2024-01-15" or date_reporting <- as.Date("2024-01-15")
dr_line_idx <- grep("^\\s*date_reporting\\s*<-", lines)

if (length(dr_line_idx) == 0) {
  stop("U 01_data_prep.R nisam našao liniju 'date_reporting <- ...'.")
}

dr_line <- lines[dr_line_idx[1]]

# Extract first YYYY-MM-DD in that line
dr_value <- stringr::str_extract(dr_line, "\\d{4}-\\d{2}-\\d{2}")

if (is.na(dr_value)) {
  stop("Našao sam date_reporting liniju, ali nisam uspio izvući datum (YYYY-MM-DD).")
}

date_reporting <- as.Date(dr_value)

# 3) Prepare data + keep only Mondays + keep up to date_reporting ----
# Ensure columns exist
needed_cols <- c("date", "value_pmmv")
missing_cols <- setdiff(needed_cols, names(df_nation2))
if (length(missing_cols) > 0) {
  stop(paste("Nedostaju kolone u CSV:", paste(missing_cols, collapse = ", ")))
}

df_tbl <- df_nation2 %>%
  mutate(
    date = as.Date(date),
    value_pmmv = as.numeric(value_pmmv)
  ) %>%
  filter(!is.na(date)) %>%
  # only Mondays (sampling days)
  filter(lubridate::wday(date, week_start = 1) == 1) %>%
  # up to reporting date
  filter(date <= date_reporting) %>%
  arrange(desc(date)) %>%
  slice_head(n = 10) %>%
  arrange(date) %>%
  transmute(
    `Sampling date (Monday)` = format(date, "%d %b %Y"),
    `National viral ratio (PMMoV-normalized)` = value_pmmv
  )

# 4) Create flextable + nice digits + test themes ----
tbl_nation_AJ_base <- flextable(df_tbl) %>%
  colformat_num(j = "National viral ratio (PMMoV-normalized)", digits = 4) %>%
  align(align = "center", part = "all") %>%
  fontsize(part = "body", size = 10) %>%
  fontsize(part = "header", size = 10) %>%
  autofit()

# Test different themes (pick one)
tbl_nation_AJ_vanilla <- tbl_nation_AJ_base %>% theme_vanilla()
tbl_nation_AJ_zebra   <- tbl_nation_AJ_base %>% theme_zebra()
tbl_nation_AJ_book    <- tbl_nation_AJ_base %>% theme_booktabs()

# Choose final theme for Quarto
tbl_nation_AJ <- tbl_nation_AJ_vanilla
# tbl_nation_AJ <- tbl_nation_AJ_zebra
# tbl_nation_AJ <- tbl_nation_AJ_book

# 5) Save for 04_quarto.R ----
if (!dir.exists(".data")) dir.create(".data")
saveRDS(tbl_nation_AJ, file = ".data/tbl_nation_last10_mondays_AJ.rds")

# Print (so you can see it in Viewer)
tbl_nation_AJ

cat(paste0("- Success : table saved -> .data/tbl_nation_last10_mondays_AJ.rds (date_reporting = ",
           as.character(date_reporting), ")\n"))