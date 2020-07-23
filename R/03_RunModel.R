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
{
  # do this on Lukas' machine:
  # Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
  remove(list = ls())
  source("setup.R")
  init_controls(
    list(
      # Data for which region should be simulated and/or fitted?
      region = "Baden-Wuerttemberg", # DO NOT WRITE BADEN-WÜRTTEMBERG HERE!
      # The umlaut seems to somehow crash everything.
      
      # Which type of data should be simulated / fitted?
      type = "Age",
      
      # How many chains and iterations should be run?
      chains = 1,
      iterations = 800,
      
      # Common or individual etas for groups (only work with Age model)
      ind_eta = FALSE,
      
      # Should the original data be plotted? Boolean.
      visualise = FALSE,
      
      # Should the Stan-model include the updating of the posteriors? If not,
      # the posteriors will not be fitted to the data! Takes on 1 (yes, should 
      # be fitted) or 0 (no, should not be fitted).
      inference = 1,
      
      # Should some information be printed for debugging?
      doprint = 0,
      
      # Should the original version (using the non-stiff ODE-solver RK45) or the
      # DTS (discrete time system) version be used?
      solver = "RK45",
      
      # How many cores do you want to use? Can be an integer from 0 to the 
      # number of cores on your machine, or "all".
      use_cores = 4
    )
  )
}
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#
{
  source("setup.R")
  source("R/99_ContactMatrix_Gender_Age_Function.R")
  
  if(check_controls() == 0) {
    warning("There was some error within the controls! Check  ",
            "messages to see where exactly.")
    stop()
  }
  
  source(paste0("R/01_DataManagement_", controls["region"], ".R"))
  source(paste0("R/02_PrepareModel_", controls["type"], ".R"))
  
  remove_except(list("controls", "data_list_model"))
}
# ----------------------------------------------------------------------------#
  
  
# ----------------------------------------------------------------------------#
# preparing and running the model ####
# ----------------------------------------------------------------------------#

# specify the number of chains and iterations to run
{
  modelPath <- paste0("Stan/", controls["type"], "_",
                      controls["solver"] ,"_",
                      controls["ind_eta"], ".stan")
  print(paste("Compiling model", modelPath))
  model_DSO = stan_model(modelPath)
  beepr::beep(10)
}


# Sampling from the posterior distribution
samples = sampling(
  model_DSO,
  data = data_list_model,
  iter = controls["iterations"],
  chains = controls["chains"],
  init = 0.5,
  control = list(max_treedepth = 10, adapt_delta = 0.8),
  cores = controls["use_cores"]
)

# Save the samples and the DSO object to RDS
{
  DSOPath <- paste0("Posteriors/", 
                    paste(controls["region"], controls["type"],
                          controls["ind_eta"], "DSO",
                          as.Date.character(Sys.time()), sep = "_"),
                    ".Rds")
  print(paste("Saving DSO to", DSOPath))
  saveRDS(object = model_DSO, 
          file   = DSOPath
  )
  PosteriorPath <- paste0("Posteriors/", 
                    paste(controls["region"], controls["type"],
                          controls["ind_eta"], controls["iterations"], "iter",
                          controls["chains"], "chains",
                          as.Date.character(Sys.time()), sep = "_"),
                    ".Rds")  
  print(paste("Saving posterior data to", PosteriorPath))
  saveRDS(object = samples,
          file   = PosteriorPath)
}



