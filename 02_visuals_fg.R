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




# save graph


