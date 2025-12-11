############################################################################### #
# Aim ----
#| load, clean and save data
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
#| list of things to do...
############################################################################### #

# Load packages ----
# select packages
pkgs <- c("dplyr", "tidyr", "zoo", "writexl", "ggplot2")
# install packages
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

# load data ----
# Belgian data are available here https://www.geo.be/catalog/details/9eec5acf-a2df-11ed-9952-186571a04de2?l=en
#| Metadata
#| siteName is the name of the treatment plant
#| collDTStart is the date of sampling
#| labName is the name of the lab analysing the sample
#| labProtocolID is the protocol used to analyse the dample
#| flowRate is the flow rate measured at the inlet of the treatment plant during sampling
#| popServ is the population covered by the treatment plant
#| measure is the target measured
#| value is the result

# sars-cov-2 data
df_sc <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantscovid&outputFormat=csv")

# pmmv data
df_pmmv <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantspmmv&outputFormat=csv")


# join both
df <- df_sc %>%
  rbind(df_pmmv)


df$collDTStart <- as.Date(df$collDTStart)
df$date <- as.Date(df$collDTStart)


# clean data
df <- df %>%
  select(siteName, collDTStart, labName, labProtocolID, flowRate, popServ, measure, value)

# format date
df$collDTStart <- as.Date(df$collDTStart)
df$date <- as.Date(df$collDTStart)


# set and subset dates
date_reporting <- as.Date("2025-09-01", format = "%Y-%m-%d")
date_graph_start <- as.Date("2024-09-01", format = "%Y-%m-%d")
date_graph_end <- as.Date("2025-12-01", format = "%Y-%m-%d")

# subset sars and pmmv data based on labProtocolID used betwen date_start and date_end
used_protocols <- df %>%
  filter(collDTStart >= date_graph_start & collDTStart <= date_graph_end) %>%
  pull(labProtocolID) %>%
  unique()
df <- df %>%
  filter(labProtocolID %in% used_protocols)

# display existing labProtocolID
unique(df$labProtocolID)

# unique(df$labProtocolID)

# rename measures
df <- df %>%
  mutate(
    measure = case_when(grepl("SARS-CoV-2", measure, ignore.case = TRUE) ~ "SARS",
                        grepl("Pepper mild mottle virus", measure, ignore.case = TRUE) ~ "PMMV",
                        TRUE ~ measure))
unique(df$measure)
# diplay existing measure

# unique(df$measure)

# translate siteName to english
df <- df %>%
  mutate(
    siteName = case_when(
      siteName == "Bruxelles-Sud" ~ "Brussels-South",
      siteName == "Bruxelles-Nord" ~ "Brussels-North",
      TRUE ~ siteName  # keep other names unchanged
    )
  )

unique(df$siteName)

# Step 1: Create Quality column BEFORE removing or changing values
df <- df %>%
  mutate(
    Quality = case_when(
      measure == "PMMV" & value < 250 ~ "Quality concerns",
      measure == "SARS" & value < 8   ~ "Quality concerns",
      is.na(value)                    ~ "Missing",
      TRUE                            ~ "OK"
    )
  )

# Step 2: Set values with "Quality concerns" to NA
df <- df %>%
  mutate(
    value = ifelse(Quality == "Quality concerns", NA_real_, value)
  )

# Step 3: Optional check — view how many values were filtered
table(df$Quality, is.na(df$value))

# remove outliers

# compute mean of replicated analysis of each measure
df_mean <- df %>%
  group_by(siteName, collDTStart, measure, popServ) %>%
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

# View result
head(df_mean)

library(tidyr)
library(dplyr)

df_ratio <- df_mean %>%
  pivot_wider(
    names_from = measure,
    values_from = mean_value
  )

# 2. Compute SARS/PMMV ratio
df_ratio <- df_ratio %>%
  mutate(
    value_pmmv = ifelse(!is.na(SARS) & !is.na(PMMV) & PMMV > 0, SARS / PMMV, NA_real_)
  )

# 3. (Optional) Convert back to long format
df_long <- df_ratio %>%
  pivot_longer(
    cols = c(SARS, PMMV, value_pmmv),
    names_to = "measure",
    values_to = "value"
  )


# unique(df$measure) ...

# compute moving average on past 14 days
# Load zoo if not already loaded
library(zoo)

# Compute 14-day moving average of the SARS/PMMV ratio per site
df_ma <- df_ratio %>%
  arrange(siteName, collDTStart) %>%
  group_by(siteName) %>%
  mutate(
    value_pmmv_ma = rollapply(
      value_pmmv,
      width = 14,
      FUN = mean,
      align = "right",
      fill = NA,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

head(df_ma %>% select(siteName, collDTStart, value_pmmv, value_pmmv_ma))


# natinoal aggregation: compute weighted mean with factor being the population served by each site
# Aggregate to national level using weighted mean (per date)
df_national <- df_ma %>%
  group_by(collDTStart) %>%
  summarize(
    national_value_pmmv_ma = weighted.mean(value_pmmv_ma, w = popServ, na.rm = TRUE),
    .groups = "drop"
  )

ref_meta <- read.csv("./Belgium_export-nation.csv")
str(ref_meta)
names(ref_meta)


df_export <- df_national %>%
  rename(date = collDTStart) %>%
  mutate(
    country = "Belgium",
    unit = "ratio",
    indicator = "SARS/PMMV 14d"
  ) %>%
  select(date, value = national_value_pmmv_ma, country, unit, indicator)


head(df_export)
write.csv(df_export, "./Belgium_export-nation_NEW.csv", row.names = FALSE)


# export data ----
# create folder if not existing

# export as csv

# export as xls

# export as rds

# Load necessary package for Excel export
library(openxlsx)

# Step 1: Create './data' folder if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Step 2: Export to CSV (.csv)
write.table(df_export, file = "./data/Belgium_export-nation.csv", sep = ",", row.names = FALSE)

# Step 3: Export to Excel (.xlsx)
write.xlsx(df_export, file = "./data/Belgium_export-nation.xlsx", rowNames = FALSE)

# Step 4: Export to RDS (.rds)
saveRDS(df_export, file = "./data/Belgium_export-nation.rds")


# display msg
cat("- Success : data prep \n")

