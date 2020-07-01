# ----------------------------------------------------------------------------#
# ModelDiagnostics.R
# 
# This idea of this file is to run model diagnostics. Functions for this file
# come from the script 04a_ModelDiagnostics_Plotting_Functions
# ----------------------------------------------------------------------------#
#
#
# ----------------------------------------------------------------------------#
# Entering region name ####
# ----------------------------------------------------------------------------#

# I'm intending to write only one diagnostic script for all regions. Please
# replace the region variable with the name of the regions you want.

region <- "Spain"

# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#
# this line is an attempt at making the code execution stop when there are 
# errors in on of the files to be sourced 
source(paste0("R/02_PrepareModel_", region, ".R"))   # contains all other parameters 
source("R/04_0ModelDiagnostics_Plotting_Functions.R") # for visualizations
theme_set(theme_bw())

# Load the saved posterior samples in the data. I don't want to source
# 03_RunModel file here as it would force the whole sampling process to repeat
# every time we call this script. 
samples <-  readRDS(file = paste0("Posteriors/",region, "_samples.Rds"))
# ----------------------------------------------------------------------------#

# Gather all paremeters in theta
pars <- c("beta","eta","epsilon","rho","pi","psi")

# Plot the parameter posteriors and overlaying the chains to check for
# consistence
stan_dens(samples, pars = pars, separate_chains = T)

plot_incidence_cases(samples = samples,
                     data_list = data_list_model, # sourced from 02_
                     start_date = day_data,
                     end_date = day_max)

plot_total_cases(samples = samples,
                     data_list = data_list_model) # sourced from 02_

