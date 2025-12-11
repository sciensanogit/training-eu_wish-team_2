############################################################################### #
# Aim ----
#| produce visuals
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

# load data
input_file <- "Belgium_export-nation.csv"
nation <- read.csv(input_file)

nation <- read.csv(input_file, sep = ";")
nation$date <- as.Date(nation$date)


# create folder if not existing
if (!dir.exists("Mission2")) {
  dir.create("Mission2")
}


# graph at national level
p <- nation |>
  arrange(date) |>
  ggplot(aes(x = date, y = value_avg14d_past)) +
  geom_line(linewidth = 1.2) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y"
  ) +
  labs(
    title = "14-day avg (value_avg14d_past)",
    x = "Date",
    y = "14-day avg"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(vjust = 0.5)
  )
print(p)

pvalue <- nation |>
  arrange(date) |>
  ggplot(aes(x = date, y = value)) +
  geom_line(linewidth = 1.2) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y"
  ) +
  labs(
    title = " Value",
    x = "Date",
    y = "Value"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(vjust = 0.5)
  )
print(pvalue)

# save graph

cat("- This script was coded by member 1 \n")

