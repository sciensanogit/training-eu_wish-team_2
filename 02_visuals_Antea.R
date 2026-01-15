#Antea
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

################ load data ################

# loadaj file koji kreira 01_data_prep-solution.R (nakon pokretanja te skripte)
# file_primary <- "./data/Belgium_export-nation.csv" to ne to nema ppmv kolona
file_fallback <- "./Belgium_export-nation.csv"

#ovaj dio je if petlja ako ne uspije uvest primary i ako ne uspije to onda uzima fileback, a ako ni to onda javi error u konzolu
if (file.exists(file_primary)) {
  df <- read.csv(file_primary)
  cat("Loaded data from ./data/Belgium_export-nation.csv\n")
} else if (file.exists(file_fallback)) {
  df <- read.csv(file_fallback)
  cat("Loaded data from ./Belgium_export-nation.csv (fallback)\n")
} else {
  stop("Data file not found. Run 01_data_prep-solution.R first.")
}

# urediti format datuma
stopifnot(is.data.frame(df))

df <- df %>%
  mutate(date = as.Date(date))

################  create folder if not existing ################ 
#kreiranje plot foldera 
if (!dir.exists("./plot")) {
  dir.create("./plot")
  cat("- Created ./plot directory\n")
}

################ graph at national level ################  
################ prepare data ################

df <- df %>%
  mutate(
    date = as.Date(date),
    value_avg14d_past = zoo::rollmean(
      value,
      k = 14,
      fill = NA,
      align = "right"
    )
  )

################ graph at national level ################  

graph_national_lvl <- ggplot(df, aes(x = date)) +
  geom_line(
    aes(y = value, colour = "Daily value"),
    linewidth = 0.8,
    alpha = 0.7
  ) +
  geom_line(
    aes(y = value_avg14d_past,
        colour = "14-day moving average"),
    linewidth = 1.2
  ) +
  scale_colour_manual(
    values = c(
      "Daily value" = "grey50",
      "14-day moving average" = "#0072B2"
    ),
    name = ""
  ) +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "2 months",
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Viral ratio at national level (Belgium)",
    x = "Date",
    y = "Viral ratio"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(graph_national_lvl)


#################  save graph ################ 
#sejvanje grafa u .png obliku u novootvoreni plot folder
ggsave(
  filename = "./plot/graph-viral_ratio-nation.png",
  plot = graph_national_lvl,
  width = 10,
  height = 6,
  dpi = 300
)

# display msg
cat("- Success : visuals saved \n")
