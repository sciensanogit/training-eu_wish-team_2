############################################################################### #
# Aim ----
#| generating wastewater reports
# Requires: 
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
#|
############################################################################### #

# Load packages ----
# select packages
pkgs <- c("dplyr", "ggplot2")
# install packages
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))


# Mission 1_1 ----
# Source member 1 script
source("mission1_1-member1.R")

# Source member 2 script
source("mission1_1-member2.R")

# Source member MG script
source("mission1_1-MG.R")

# Source member AJ script
source("mission1_AJ.R")

# Source member FG script
source("mission1_FG.R")

# Source member LD script


#Session 2

#source 01_data_prep.R
source("1_data_prep_LD.R")

#source 02_visuals.R
source("02_visuals_fg.R")

#source 03_tbl.R
source("03_visuals_AJ.R")

#source 04_quarto


# Mission 1_2 ----
# produce visuals
source("mission1_2.R")

# Mission 2 ----
# produce reports
source("mission2.R")


