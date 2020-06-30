# ----------------------------------------------------------------------------#
# RunModel_Spain.R
# 
# This idea of this file is to only run the model. The computation of variables
# should happen in a separate file that has to be called before.
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#
# this line is an attempt at making the code execution stop when there are 
# errors in on of the files to be sourced 
source("R/02_PrepareModel_Spain.R")   # contains all other parameters
# ----------------------------------------------------------------------------#



# ----------------------------------------------------------------------------#
# preparing and running the model ####
# ----------------------------------------------------------------------------#

# Creating DSO object
spain_DSO = stan_model(file = "Stan/all_regions_Stan_model.stan")


# Sampling from the posterior distribution
spain_samples = sampling(spain_DSO,data = data_list_model16A,iter = 100,chains = 2,
                     init=0.5, control=list(max_treedepth=10,adapt_delta=0.8))

# Save the samples and the DSO object to RDS
saveRDS(object = spain_DSO, file = "Postspain_DSO.Rds")
