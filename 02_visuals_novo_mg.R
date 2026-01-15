###############################################################################
# 02_visuals.R — Viral ratio national (PMMoV) + 14d past MA
###############################################################################

# Packages
pkgs <- c("dplyr", "ggplot2", "scales")
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, library, character.only = TRUE))

# 1) Load data (try ; then ,)
input_file <- "Belgium_export-nation.csv"

nation <- tryCatch(
  read.csv(input_file, sep = ";"),
  error = function(e) read.csv(input_file, sep = ",")
)

# 2) Find the date column (because yours is NOT named 'date')
date_col <- names(nation)[grepl("date|datum|time", names(nation), ignore.case = TRUE)][1]
if (is.na(date_col)) {
  stop(paste0(
    "Ne mogu pronaći stupac s datumom. Stupci su: ",
    paste(names(nation), collapse = ", ")
  ))
}

# Create a standard 'date' column safely
nation$date <- as.Date(nation[[date_col]])

# 3) Ensure the needed variables exist
# Task wants: value_pmmv and value_pmmv_avg14d_past
# If your file uses generic names (value / value_avg14d_past), map them.

if (!"value_pmmv" %in% names(nation) && "value" %in% names(nation)) {
  nation$value_pmmv <- nation$value
}
if (!"value_pmmv_avg14d_past" %in% names(nation) && "value_avg14d_past" %in% names(nation)) {
  nation$value_pmmv_avg14d_past <- nation$value_avg14d_past
}

# Final check
needed <- c("date", "value_pmmv", "value_pmmv_avg14d_past")
missing <- setdiff(needed, names(nation))
if (length(missing) > 0) {
  stop(paste0(
    "Nedostaju stupci: ", paste(missing, collapse = ", "), "\n",
    "Postojeći stupci: ", paste(names(nation), collapse = ", ")
  ))
}

# Convert to numeric + clean
nation <- nation |>
  mutate(
    value_pmmv = as.numeric(value_pmmv),
    value_pmmv_avg14d_past = as.numeric(value_pmmv_avg14d_past)
  ) |>
  filter(!is.na(date), !is.na(value_pmmv)) |>
  arrange(date)

# 4) Create ./plot folder
if (!dir.exists("plot")) dir.create("plot", recursive = TRUE)

# 5) Plot (bars + MA line)
p_viral_ratio <- ggplot(nation, aes(x = date)) +
  geom_col(aes(y = value_pmmv), alpha = 0.6) +
  geom_line(aes(y = value_pmmv_avg14d_past), linewidth = 1.2) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  labs(
    title    = "Viral ratio at national level",
    subtitle = "Daily values (bars) and past two weeks moving average (line)",
    x        = "Date",
    y        = "Viral ratio (PMMoV-normalised)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(vjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(p_viral_ratio)

# 6) Save with required name
ggsave(
  filename = "plot/graph-viral_ratio-nation.png",
  plot = p_viral_ratio,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Saved: plot/graph-viral_ratio-nation.png\n")
