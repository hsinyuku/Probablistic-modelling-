# ----------------------------------------------------------------------------#
# ModelDiagnostics.R
# 
# This script generates composite plots to be used in the paper.
# ----------------------------------------------------------------------------#

# get data for all regions
controls <- list("visualise" = F, "inference" = F, "doprint" = F) 
source("R/99_ContactMatrix_Gender_Age_Function.R")
source("setup.R")
{
  realDataRegions <- list("Austria", "BadenW", "Bavaria", "Hubei", "Lombardy",
                          "Switzerland", "Spain")
  names(realDataRegions) <- list("Austria", "BadenW", "Bavaria", "Hubei", "Lombardy",
                                 "Switzerland", "Spain")
  for(region in realDataRegions) {
    controls[["region"]] <- region
    source(paste0("R/01_DataManagement_", region, ".R"))
    controls[["type"]] <- "Age"
    source("R/02_PrepareModel_Age.R")
    dlmAge <- data_list_model
    source("R/02_PrepareModel_Gender.R")
    controls[["type"]] <- "Gender"
    dlmGender <- data_list_model
    realDataRegions[[region]] <- list("day_max" = day_max,
                                      "day_start" = day_start,
                                      "dlmAge" = dlmAge,
                                      "dlmGender" = dlmGender)
  }
}

controlsRegions <- list()
for(file in list.files("Posteriors")) {
  controlsRegions <- append(controlsRegions,
                            list(extractPosteriorMetadata(file)))
}
remove_except(list("realDataRegions", "controlsRegions", 
                   "generatedQuantitiesList"))
source("setup.R")
source("R/99_PlotFunctions.R")
source("R/99_DataFunctions.R")

generatedQuantitiesList <- readRDS("data/00_generatedQuantities.Rds")


