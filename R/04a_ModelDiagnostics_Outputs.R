# ----------------------------------------------------------------------------#
# ModelDiagnostics.R
# 
# This idea of this file is to run model diagnostics. Functions for this file
# come from the script 04a_ModelDiagnostics_Plotting_Functions
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# sourcing other scripts, setting controls ####
# ----------------------------------------------------------------------------#
# this line is an attempt at making the code execution stop when there are 
# errors in on of the files to be sourced
{
  remove(list = ls())
  source("setup.R")
  source("R/00_ContactMatrix_Gender_Age_Function.R")
  init_controls(list(region = "Spain", type = "Gender", visualise = FALSE,
                     savePlots = FALSE))
  source(paste0("R/01_DataManagement_", controls["region"], ".R"))
  source(paste0("R/02_PrepareModel_", controls["type"], ".R"))
  source("R/04b_ModelDiagnostics_Functions.R") # for visualizations
}
#theme_set(theme_bw())

# Load the saved posterior samples in the data (obviously without sampling
# them every time).
samples <-  readRDS(file = paste0("Posteriors/Spain_Samples_200718.Rds"))
sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
  # sampler_params returns a list with one entry per chain
controls["chains"] <- length(sampler_params)
  # length of the list is number of chains
controls["iterations"] <- nrow(sampler_params[[3]])
  # each row in each element of the list is one iteration without warmup

# Gather all parameters in theta
pars <- c("beta", "epsilon","rho","pi","psi", "eta")
# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#
# model inspection ####
# ----------------------------------------------------------------------------#
{
  title = paste0("Posterior Density Plots (", controls["region"], ")")
  subtitle = ("For 4 separating chains, 1000 iterations / chain")
  plot <- stan_dens(samples, pars = pars, separate_chains = T, nrow = 3) +
    labs(title = title, subtitle = subtitle) +
    scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))
  if (controls["savePlots"]) {
    save_gg(plot = plot, name = "Posterior Density")
  }
}
