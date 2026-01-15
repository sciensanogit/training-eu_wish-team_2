###############################################################################
# Aim ----
#| generating wastewater reports
###############################################################################

# Load packages ----
pkgs <- c("dplyr", "ggplot2", "flextable", "quarto", "readr")
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

###############################################################################
# 0) Ensure ./plot exists ------------------------------------------------------
###############################################################################
if (!dir.exists("plot")) dir.create("plot", recursive = TRUE)

###############################################################################
# 1) Ensure graph_oostend_aalst.png exists (create fake if missing) -----------
###############################################################################
png_path <- file.path("plot", "graph_oostend_aalst.png")

if (!file.exists(png_path)) {
  message("graph_oostend_aalst.png not found. Creating a fake placeholder plot...")
  
  # fake data
  set.seed(1)
  df_fake <- data.frame(
    date = seq.Date(Sys.Date() - 90, Sys.Date(), by = "day"),
    value = cumsum(rnorm(91))
  )
  
  p_fake <- ggplot(df_fake, aes(x = date, y = value)) +
    geom_line(linewidth = 1.2) +
    labs(
      title = "Placeholder: Oostende vs Aalst",
      subtitle = "This is a fake plot created because the expected PNG was missing.",
      x = "Date",
      y = "Arbitrary units"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(png_path, p_fake, width = 10, height = 6, dpi = 150)
  message("Saved placeholder PNG to: ", png_path)
}

###############################################################################
# 2) Define date_reporting (prefer existing variable, else from data) ----------
###############################################################################
if (!exists("date_reporting")) {
  
  preferred_file <- file.path("data", "Belgium_export-nation.csv")
  fallback_file  <- "Belgium_export-nation.csv"
  input_file <- if (file.exists(preferred_file)) preferred_file else fallback_file
  
  if (!file.exists(input_file)) {
    stop("Cannot define date_reporting: missing ./data/Belgium_export-nation.csv and ./Belgium_export-nation.csv")
  }
  
  # your export is usually ';' separated
  df_nation <- tryCatch(
    read.csv(input_file, sep = ";", header = TRUE, stringsAsFactors = FALSE),
    error = function(e) read.csv(input_file, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  )
  
  # find date column robustly (in case it isn't exactly 'date')
  date_col <- names(df_nation)[grepl("date|datum|time", names(df_nation), ignore.case = TRUE)][1]
  if (is.na(date_col)) stop("Could not find a date column in the nation export.")
  
  df_nation[[date_col]] <- as.Date(df_nation[[date_col]])
  date_reporting <- max(df_nation[[date_col]], na.rm = TRUE)
}

###############################################################################
# 3) Load epi assessment text -------------------------------------------------
###############################################################################
main_text <- read.csv("epi_assessment_text.csv", stringsAsFactors = FALSE)

###############################################################################
# 4) Load tbl_nation saved by team (if exists) --------------------------------
###############################################################################
tbl_path_candidates <- c(
  file.path("data", "tbl_nation_last10_mondays.rds"),
  file.path("data", "tbl_nation_viral_ratio_last10_mondays.rds")
)

tbl_path <- tbl_path_candidates[file.exists(tbl_path_candidates)][1]

if (!is.na(tbl_path)) {
  tbl_nation <- readRDS(tbl_path)
  message("Loaded tbl_nation from: ", tbl_path)
} else {
  tbl_nation <- NULL
  message("tbl_nation .rds not found yet (OK for now).")
}

###############################################################################
# 5) Render weekly report ------------------------------------------------------
###############################################################################
save.image(".RData")

output_name <- paste0("Report-", format(date_reporting, "%G-W%V"), "-mg")
format_output <- c("html")

out <- quarto::quarto_render(
  input = "04_quarto.qmd",
  output_file = output_name,
  output_format = format_output
)

cat("- Success : quarto render\n")

# Open HTML in browser (robust)
html_file <- paste0(output_name, ".html")

if (file.exists(html_file)) {
  browseURL(normalizePath(html_file))
} else {
  warning("HTML file not found: ", html_file)
}
