############################################################################### #
# Aim ----
#| generating wastewater reports
# Requires: 
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
############################################################################### #

# Load packages ----
# select packages
pkgs <- c("dplyr", "ggplot2", "flextable", "quarto")
# install packages
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

# Save table ----
tbl_oostende_aalst <- df %>%
  filter(labProtocolID == "SC_COV_4.1") %>%
  filter(measure == "SARS-CoV-2 E gene") %>%
  filter(date > date_graph_start & date < date_reporting) %>%
  filter(siteName %in% c("Aalst", "Oostende")) %>%
  select(siteName, date, measure, value) %>%
  arrange(desc(date)) %>% 
  slice_head(n = 10) %>%
  flextable() %>%
  fontsize(part = "body",size = 10) %>%
  fontsize(part = "header",size = 10) %>%
  autofit() %>% theme_vanilla()

# Render weekly sub report ----
save.image(".RData")
quarto_render("mission2.qmd", output_format = "html")
