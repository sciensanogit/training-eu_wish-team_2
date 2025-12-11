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

df$collDTStart <- as.Date(df$collDTStart)
df$date <- as.Date(df$collDTStart)

# sars-cov-2 data
df_sc <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantscovid&outputFormat=csv")

# pmmv data
df_pmmv <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantspmmv&outputFormat=csv")

# join both
df <- df_sc %>%
  rbind(df_pmmv)

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

# apply LOQ provided by the lab
df <- df %>%
  mutate(
    value = case_when(
      measure == "PMMV" & value < 250 ~ NA_real_,
      measure == "SARS" & value < 8   ~ NA_real_,
      TRUE ~ value
    )
  )
table(df$measure, is.na(df$value))


# remove outliers

# compute mean of replicated analysis of each measure

# compute viral ratio
# unique(df$measure) ...

# compute moving average on past 14 days

# natinoal aggregation: compute weighted mean with factor being the population served by each site

# export data ----
# create folder if not existing

# export as csv

# export as xls

# export as rds


# display msg
cat("- Success : data prep \n")

