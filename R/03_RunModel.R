# ----------------------------------------------------------------------------#
# RunModel.R
# 
# This idea of this file is to only run the model. The computation of variables
# should happen in a separate file that has to be called before.
# ----------------------------------------------------------------------------#


library(stringr)

# ----------------------------------------------------------------------------#
# controls  ####
# ----------------------------------------------------------------------------#
{
  remove(list = ls())
  source("setup.R")   # contains all other parameters
  
  # Which type of data should be simulated / fitted?
  type = "Age"
  
  # Data for which region should be simulated and/or fitted?
  region = "Spain"
  
  # How many chains and iterations should be run?
  chains = 1
  iterations = 200
  
  # Should the original data be plotted? Boolean.
  visualise = FALSE
  
  # Should the Stan-model include the updating of the posteriors? If not, the
  # posteriors will not be fitted to the data! Takes on 1 (yes, should be fitted)
  # or 0 (no, should not be fitted).
  inference = 1
  
  # Should some information be printed for debugging?
  doprint = 0
  
  # Should the original version (using the non-stiff ODE-solver RK45) or the
  # DTS (discrete time system) version be used?
  solver = "DTS"
}
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#
# sourcing scripts, checking controls ####
# ----------------------------------------------------------------------------#
{
  # checking controls --------------------------------------------------------#
  type = stringr::str_to_title(type)
  
  if(region == "Baden-Württemberg") region = "BadenW"
  if (!(region %in% regions)) warning(
    "The region you specified is not a correct string.\nFunctions will not ",
    "work! Please change the string. \nCheck the regions-object for ",
    "the correct spelling of the regions.")
  
  # inference must be in integer, not a boolean
  if (class(inference) == "logical") inference = as.integer(inference)
  if (!(inference %in% c(0, 1))) warning(
    "Inference must be either 1 or 0!"
  )
  
  source(paste0("R/01_DataManagement_", region, ".R"))
  
  # Source the prepare model file --------------------------------------------#
  source(paste0("R/02_PrepareModel_", type, ".R"))
  
  remove(list = ls()[!(ls() %in% list("region", "type", "visualise", "inference",
                                      "doprint", "iterations","data_list_model",
                                      "chains", "solver"))]) 
}
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# preparing and running the model ####
# ----------------------------------------------------------------------------#

# specify the number of chains and iterations to run


model_DSO = stan_model(paste0("Stan/",type, "_", solver, ".stan"))

# Sampling from the posterior distribution
samples = sampling(
  model_DSO,
  data = data_list_model,
  iter = iterations,
  chains = chains,
  init = 0.5,
  control = list(max_treedepth = 10, adapt_delta = 0.8)
)

# Save the samples and the DSO object to RDS
saveRDS(object = model_DSO, 
        file = paste0("Posteriors/", 
                      paste(region, "DSO", type, "iter", iterations, "chains",
                            chains, as.Date.character(Sys.time()), sep = "_"),
                      ".Rds")
        )

saveRDS(object = samples,
        file = paste0("Posteriors/", 
                      paste(region, "samples", type, "iter", iterations, "chains",
                            chains, as.Date.character(Sys.time()), sep = "_"),
                      ".Rds")
        )
