# ----------------------------------------------------------------------------#
# ModelDiagnostics.R
# 
# This idea of this file is to run model diagnostics on any posterior.
# ----------------------------------------------------------------------------#

remove(list = ls())
source("setup.R")
source("R/99_ContactMatrix_Gender_Age_Function.R")
source("R/04_ModelDiagnostics_Regions.R")

# Print the posteriors that exist:
list.files("Posteriors")

initialiseSession("Lombardy", "Gender", FALSE, 1000, 4, "2020-07-21",
                  FALSE, TRUE)
initialisePosterior()


