###############################################################################
# 04_quarto.R — generating wastewater reports (+ extras)
###############################################################################
# --- HARD FIX for prettyNum(trim) errors ---
if (exists("trim", inherits = TRUE)) rm(trim, inherits = TRUE)

# detach scales if it was loaded in this session
if ("package:scales" %in% search()) {
  detach("package:scales", unload = TRUE, character.only = TRUE)
}

# Packages ----
pkgs <- c("dplyr", "ggplot2", "flextable", "quarto", "lubridate")
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

# folders ----
if (!dir.exists("plot")) dir.create("plot", recursive = TRUE)
if (!dir.exists("data")) dir.create("data", recursive = TRUE)

###############################################################################
# Helpers ---------------------------------------------------------------------
###############################################################################

read_nation_export <- function() {
  preferred <- file.path("data", "Belgium_export-nation.csv")
  fallback  <- "Belgium_export-nation.csv"
  input_file <- if (file.exists(preferred)) preferred else fallback
  if (!file.exists(input_file)) stop("Nation export not found in ./data or project root.")
  
  # your export is usually ';'
  df <- tryCatch(
    read.csv(input_file, sep = ";", header = TRUE, stringsAsFactors = FALSE),
    error = function(e) read.csv(input_file, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  )
  # type cleanup
  if ("date" %in% names(df)) df$date <- as.Date(df$date)
  if ("value_pmmv" %in% names(df)) df$value_pmmv <- as.numeric(df$value_pmmv)
  if ("value_pmmv_avg14d_past" %in% names(df)) df$value_pmmv_avg14d_past <- as.numeric(df$value_pmmv_avg14d_past)
  df
}

make_placeholder_png <- function(path) {
  set.seed(1)
  df_fake <- data.frame(
    date = seq.Date(Sys.Date() - 90, Sys.Date(), by = "day"),
    value = cumsum(rnorm(91))
  )
  p_fake <- ggplot(df_fake, aes(date, value)) +
    geom_line(linewidth = 1.1) +
    labs(
      title = "Placeholder graph",
      subtitle = "Auto-created because expected PNG was missing",
      x = "Date", y = "Arbitrary units"
    ) +
    theme_minimal(base_size = 12)
  ggsave(path, p_fake, width = 10, height = 6, dpi = 150)
}

###############################################################################
# 1) Ensure required PNG exists (or create a fake one) -------------------------
###############################################################################
png_needed <- file.path("plot", "graph_oostend_aalst.png")
if (!file.exists(png_needed)) {
  message("Missing ./plot/graph_oostend_aalst.png -> creating a fake one.")
  make_placeholder_png(png_needed)
}

###############################################################################
# 2) Load data + main text -----------------------------------------------------
###############################################################################
df_nation <- read_nation_export()

main_text <- read.csv("epi_assessment_text.csv", stringsAsFactors = FALSE)

# Load tbl_nation when your team has saved it (optional) ----------------------
tbl_path_candidates <- c(
  file.path("data", "tbl_nation_last10_mondays.rds"),
  file.path("data", "tbl_nation_viral_ratio_last10_mondays.rds"),
  file.path("data", "tbl_nation_last10_mondays.rds")
)
tbl_path <- tbl_path_candidates[file.exists(tbl_path_candidates)][1]
tbl_nation <- if (!is.na(tbl_path)) readRDS(tbl_path) else NULL

###############################################################################
# 3) Extra: Brussels-North table (last 10 dates) -------------------------------
###############################################################################
tbl_brussels_north <- NULL
if ("siteName" %in% names(df_nation)) {
  df_bn <- df_nation %>%
    filter(siteName == "Brussels-North") %>%
    filter(!is.na(date)) %>%
    arrange(desc(date)) %>%
    slice_head(n = 10) %>%
    arrange(date)
  
  if (nrow(df_bn) > 0) {
    # if value_pmmv not present, try fallback to "value"
    if (!"value_pmmv" %in% names(df_bn) && "value" %in% names(df_bn)) {
      df_bn$value_pmmv <- as.numeric(df_bn$value)
    }
    
    tbl_brussels_north <- df_bn %>%
      transmute(
        `Sampling date` = date,
        `Viral ratio (PMMoV-normalised)` = as.numeric(value_pmmv)
      ) %>%
      flextable() %>%
      colformat_num(j = "Viral ratio (PMMoV-normalised)", digits = 2) %>%
      theme_booktabs() %>%
      autofit()
  }
}

###############################################################################
# 4) Extra: Influenza + RSV graphs (Sep–Dec 2025) -----------------------------
###############################################################################
make_sep_dec_plot <- function(url, protocols, outfile, title_txt) {
  df <- read.csv(url, stringsAsFactors = FALSE)
  
  # try to find likely columns
  date_col <- names(df)[grepl("^date$|sample.*date|collection.*date", names(df), ignore.case = TRUE)][1]
  val_col  <- names(df)[grepl("^value$|result|measurement|concentration", names(df), ignore.case = TRUE)][1]
  
  if (is.na(date_col) || is.na(val_col)) {
    # If schema differs, still save a placeholder explaining issue
    p <- ggplot() +
      annotate("text", x = 0, y = 0,
               label = paste0("Could not auto-detect date/value columns.\nFound columns:\n",
                              paste(head(names(df), 20), collapse = ", "))) +
      theme_void() +
      labs(title = title_txt)
    ggsave(outfile, p, width = 10, height = 6, dpi = 150)
    return(invisible(NULL))
  }
  
  df[[date_col]] <- as.Date(df[[date_col]])
  
  df_f <- df %>%
    filter(labProtocolID %in% protocols) %>%
    filter(df[[date_col]] >= as.Date("2025-09-01") & df[[date_col]] <= as.Date("2025-12-31")) %>%
    mutate(value_num = suppressWarnings(as.numeric(gsub(",", ".", df[[val_col]])))) %>%
    filter(!is.na(df[[date_col]]), !is.na(value_num))
  
  # if empty, create an informative placeholder
  if (nrow(df_f) == 0) {
    p <- ggplot() +
      annotate("text", x = 0, y = 0,
               label = "No rows after filtering (protocol/date range).") +
      theme_void() +
      labs(title = title_txt)
    ggsave(outfile, p, width = 10, height = 6, dpi = 150)
    return(invisible(NULL))
  }
  
  p <- ggplot(df_f, aes(x = .data[[date_col]], y = value_num)) +
    geom_line(linewidth = 1.1) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
    scale_y_continuous() +
    labs(title = title_txt, x = "Date", y = "Value") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
  
  
  ggsave(outfile, p, width = 10, height = 6, dpi = 150)
}

# Influenza
make_sep_dec_plot(
  url = "https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantsinfluenza&outputFormat=csv",
  protocols = c("SC_INF_2.0", "UA_INF_2.0"),
  outfile = file.path("plot", "graph-influenza-sep-dec-2025.png"),
  title_txt = "Influenza (Sep–Dec 2025) — SC_INF_2.0 & UA_INF_2.0"
)

# RSV
make_sep_dec_plot(
  url = "https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantsrsv&outputFormat=csv",
  protocols = c("SC_RSV_2.0", "UA_RSV_2.0"),
  outfile = file.path("plot", "graph-rsv-sep-dec-2025.png"),
  title_txt = "RSV (Sep–Dec 2025) — SC_RSV_2.0 & UA_RSV_2.0"
)

###############################################################################
# 5) LOQ coloring on national viral ratio graph (below 1000 red, above green) --
###############################################################################
make_national_loq_plot <- function(df, date_reporting, outfile) {
  dfp <- df %>%
    filter(!is.na(date)) %>%
    filter(date <= date_reporting) %>%
    arrange(date)
  
  # if file uses generic "value" for viral ratio, map it
  if (!"value_pmmv" %in% names(dfp) && "value" %in% names(dfp)) dfp$value_pmmv <- as.numeric(dfp$value)
  if (!"value_pmmv_avg14d_past" %in% names(dfp) && "value_avg14d_past" %in% names(dfp)) {
    dfp$value_pmmv_avg14d_past <- as.numeric(dfp$value_avg14d_past)
  }
  
  # LOQ flag
  dfp <- dfp %>%
    mutate(
      loq_flag = ifelse(value_pmmv < 1000, "Below LOQ (<1000)", "Above LOQ (≥1000)")
    )
  
  p <- ggplot(dfp, aes(x = date)) +
    geom_point(aes(y = value_pmmv, color = loq_flag), size = 2, alpha = 0.9, na.rm = TRUE) +
    geom_line(aes(y = value_pmmv_avg14d_past), linewidth = 1.1, na.rm = TRUE) +
    scale_color_manual(values = c("Below LOQ (<1000)" = "red3", "Above LOQ (≥1000)" = "green4")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
    scale_y_continuous() +
    labs(
      title = "National viral ratio (PMMoV-normalised)",
      x = "Date",
      y = "Viral ratio",
      color = "Quantification"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
  
  
  
  ggsave(outfile, p, width = 10, height = 6, dpi = 300)
}

###############################################################################
# 6) Produce TWO reports for two different dates -------------------------------
###############################################################################
dates_to_render <- as.Date(c("2025-08-24", "2025-09-01"))

for (date_reporting in dates_to_render) {
  
  # make sure national LOQ plot ends at date_reporting
  make_national_loq_plot(
    df = df_nation,
    date_reporting = date_reporting,
    outfile = file.path("plot", "graph-viral_ratio-nation.png")  # overwrite so qmd always picks it up
  )
  
  # Save objects for QMD
  save.image(".RData")
  
  # output: add -mg + date so files are unique
  output_name <- paste0("Report-", format(date_reporting, "%Y-%m-%d"), "-mg")
  
  # render BOTH html + docx
  quarto::quarto_render(
    input = "04_quarto.qmd",
    output_file = output_name,
    output_format = c("html", "docx")
  )
  
  cat("- Success : quarto render for ", as.character(date_reporting), "\n")
  
  # open HTML
  html_file <- paste0(output_name, ".html")
  if (file.exists(html_file)) browseURL(normalizePath(html_file))
}
