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
nation <- read.csv("Belgium_export-nation.csv")
if (!dir.exists("plot")) {
  dir.create("plot")
}

# graph at national level
# Convert date column
nation$date <- as.Date(nation$date)

# Ensure value is numeric
nation$value <- as.numeric(nation$value)

# Remove missing values
nation <- nation |> filter(!is.na(date), !is.na(value))

# Create plot folder if not existing
if (!dir.exists("plot")) {
  dir.create("plot")
}

p_viral_ratio <- ggplot(nation, aes(x = date)) +
  geom_col(aes(y = value_pmmv), alpha = 0.6) +
  geom_line(aes(y = value_pmmv_avg14d_past), linewidth = 1.2) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    title    = "Viral ratio at national level",
    subtitle = "Daily values and past two weeks moving average",
    x        = "Date",
    y        = "Viral ratio (PMMoV-normalised)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 11),
    axis.text.x     = element_text(vjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(p_viral_ratio)

# Save plot
ggsave(
  filename = "plot/graph-national-level.png",
  plot = p_national,
  width = 10,
  height = 6,
  dpi = 300
)

names(nation)
head(nation, 3)
