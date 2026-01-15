############################################################################### #
# Aim ----
#| produce visuals
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
#| list of things to do...
############################################################################### #

# Load packages ----
pkgs <- c("dplyr", "ggplot2", "readr", "scales")
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, library, character.only = TRUE))

# load data ----
path1 <- ".data/Belgium_export-nation.csv"
path2 <- "Belgium_export-nation.csv"

if (file.exists(path1)) {
  df <- readr::read_delim(path1, delim = ";", show_col_types = FALSE)
} else if (file.exists(path2)) {
  df <- readr::read_delim(path2, delim = ";", show_col_types = FALSE)
} else {
  stop("Ne mogu naći CSV. Očekujem: .data/Belgium_export-nation.csv ili Belgium_export-nation.csv")
}

# prepare data ----
df <- df %>%
  mutate(
    date = as.Date(date),
    value_pmmv = as.numeric(value_pmmv),
    value_pmmv_avg14d_past = as.numeric(value_pmmv_avg14d_past)
  ) %>%
  arrange(date)

# create plot folder if not existing ----
if (!dir.exists("plot_AJ")) dir.create("plot_AJ")

# graph at national level ----
p <- ggplot(df, aes(x = date)) +
  geom_line(aes(y = value_pmmv, color = "Raw value"),
            linewidth = 0.5, alpha = 0.9) +
  geom_point(aes(y = value_pmmv, color = "Raw value"),
             size = 1.2) +
  geom_line(aes(y = value_pmmv_avg14d_past,
                color = "14-day past moving average"),
            linewidth = 1.2) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Raw value" = "#2C7BB6",
      "14-day past moving average" = "#D7191C"
    )
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Belgium – Viral ratio (national level) – AJ",
    x = "Date",
    y = "Viral ratio (PMMoV-normalized)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )


# save graph ----
ggsave(
  filename = "plot_AJ/graph-viral_ratio-nation_AJ.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)

# display msg ----
cat("- Success : visuals saved \n")