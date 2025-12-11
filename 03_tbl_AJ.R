############################################################################### #
# Aim ----
#| produce tables
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
#| list of things to do...
############################################################################### #

# Load packages ----
# select packages
pkgs <- c("dplyr", "ggplot2", "flextable", "quarto", "lubridate")
# install packages
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

date_reporting <- NULL

# load data
df_nation <- read.table(
  file = "./Belgium_export-nation.csv", 
  sep = ";", 
  dec = ".", 
  header = TRUE)

#DATA PREP
df_tbl_data <- df_nation %>%
  #convert date to date format
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  
  #define max date
  {
    date_reporting <<- max(.$date)
    .
  } %>%
  
  #filter day of week
  mutate(day_of_week = wday(date, label = TRUE, week_start = 1, locale = "en")) %>%
  
  #filter monday
  filter(day_of_week == "Mon") %>%
  

  select(date, value_pmmv) %>%
  
  arrange(desc(date)) %>%
  
  slice_head(n = 10)

tbl_nation <- df_tbl_data %>%
  flextable() %>%
  
  set_header_labels(
    date = "Sampling Date",
    value_pmmv = "National Viral Ratio (SARS/PMMV)"
  ) %>%
  
  colformat_date(j = "date", fmt = "%Y-%m-%d") %>%
  
  colformat_num(j = "value_pmmv", digits = 3) %>%
  
  fontsize(part = "all", size = 10) %>%
  autofit() %>%
  
  theme_zebra()

print(tbl_nation)

save_as_docx(tbl_nation, path = "./national_viral_ratio_last_10_mondays_table.docx")

# display msg
cat("- Success : tables saved \n")

df_date_check <- df_nation %>%
  mutate(date_converted = as.Date(date, format = "%Y-%m-%d"))

print(dim(df_date_check))
print(head(df_date_check))

print(sum(is.na(df_date_check$date_converted)))