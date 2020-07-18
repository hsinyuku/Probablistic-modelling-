# ----------------------------------------------------------------------------#
# RunModel.R
# 
# This idea of this file is to only run the model. The computation of variables
# should happen in a separate file that has to be called before.
# ----------------------------------------------------------------------------#

# run the whole block to get the necessary data for the Stan-model

# ----------------------------------------------------------------------------#
# controls  ####
# ----------------------------------------------------------------------------#
source("setup.R")   # contains all other parameters
# the following parameters all determine which parts of the code are executed
# (or not). 
# Data for which region should be simulated and/or fitted?
region = "Bavaria"

# Should the data be differentiated according to age groups or gender? Takes
# either "age" or "gender"
type = "gender"

# ----------------------------------------------------------------------------#
if(region == "Baden-Württemberg") region = "BadenW"
if (!(region %in% regions)) warning(
  "The region you specified is not a correct string.\nFunctions will not ",
  "work! Please change the string. \nCheck the regions-object for ",
  "the correct spelling of the regions.")

# Should the original data be plotted? Boolean.
visualise = FALSE

# Should the Stan-model include the updating of the posteriors? If not, the
# posteriors will not be fitted to the data! Takes on 1 (yes, should be fitted)
# or 0 (no, should not be fitted).
inference = 1
if (class(inference) == "logical") inference = as.integer(inference)
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#
# Cleaning the global environment so no data from other runs of the code can 
# influence the current run.
remove(list = ls()[!(ls() %in% list("region", "type", "visualise", "inference"))]) 
  # clearing the work space, except for the necessary control parameters
source(paste0("R/01_DataManagement_", region, ".R"))

# Source the right prepare model file
if (type == "age") {
  source("R/02_PrepareModel_Age.R", echo = T)
} else if (type == "gender") {
  source("R/02_PrepareModel_Gender.R", echo = T)
}
# Works with:
#   Austria
#   Spain
#   Baden-Württemberg
#   Bavaria
#   Hubei
#   Switzerland
#   Lombardy
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# preparing and running the model ####
# ----------------------------------------------------------------------------#
model_DSO = stan_model(paste0("Stan/all_regions_Stan_", type, ",_model.stan"))

# Sampling from the posterior distribution
samples = sampling(
  model_DSO,
  data = data_list_model,
  iter = 300,
  chains = 3,
  init = 0.5,
  warmup = 50,
  control = list(max_treedepth = 10, adapt_delta = 0.8)
)

# Save the samples and the DSO object to RDS
saveRDS(object = model_DSO, file = paste0("Posteriors/", 
                                          region, "_DSO_", 
                                          type, "_", 
                                          as.Date.character(Sys.time()),
                                          ".Rds"))

saveRDS(object = samples, file = paste0("Posteriors/", 
                                        region, "_samples_", 
                                        type, "_", 
                                        as.Date.character(Sys.time()),
                                        ".Rds"))






