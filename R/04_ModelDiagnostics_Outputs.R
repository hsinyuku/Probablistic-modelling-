# ----------------------------------------------------------------------------#
# ModelDiagnostics.R
# 
# This idea of this file is to run model diagnostics. Functions for this file
# come from the script 04a_ModelDiagnostics_Plotting_Functions
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# sourcing other scripts, setting controls ####
# ----------------------------------------------------------------------------#
{
  remove(list = ls())
  source("setup.R")
  source("R/99_ContactMatrix_Gender_Age_Function.R")
  # specify here which posterior file you want to load!
  init_controls(list(region = "Spain",
                     type = "age",
                     visualise = FALSE,
                     savePlots = FALSE,
                     ind_eta = FALSE,
                     chains = 3,
                     iterations = 200,
                     timestamp = "2020-07-18",
                     # these controls don't do anything, but need to have some value
                     inference = 1,
                     use_cores = 1))
  # this verifies your input to init_controls
  if(check_controls() == 0) {
    warning("There was some error within the controls! Check  ",
            "messages to see where exactly.")
    stop()
  }
  source(paste0("R/01_DataManagement_", controls["region"], ".R"))
  source(paste0("R/02_PrepareModel_", controls["type"], ".R"))
  remove_except(list("controls", "data_list_model", "day_data", "day_max"))
  source("setup.R") # adding necessary functions back in
  source("R/99_PlotFunctions.R") # for visualizations
}
theme_set(theme_bw())
{
  # Load the saved posterior samples in the data (obviously without sampling
  # them every time).
  postPath <- paste0("Posteriors/", paste(controls$region, 
                                          str_to_title(controls$type),
                                          controls$ind_eta,
                                          controls$iterations, "iter",
                                          controls$chains, "chains",
                                          controls$timestamp,
                                          sep = "_"),
                     ".Rds")
  if(!file.exists(postPath)) {
    stop(paste0("The posterior you chose does not exist (on your machine? ",
                "on this branch?). Remember that you have to download the ",
                "posteriors manually from GDrive!"))
  }
  samples <-  readRDS(file = postPath)
  sampler_params <- get_sampler_params(samples, inc_warmup = TRUE)
  # sampler_params returns a list with one entry per chain
  controls["chains"] <- length(sampler_params)
  # length of the list is number of chains
  controls["iterations"] <- nrow(sampler_params[[1]])
  # each row in each element of the list is one iteration without warmup
  controls["warmup"] <- controls$iterations -
    nrow(get_sampler_params(samples, inc_warmup = FALSE)[[1]])
}
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# real data ####
# ----------------------------------------------------------------------------#

# numbers of cases /deaths per day -------------------------------------------#
plot_Real_Time("deaths", day_data, day_max)
plot_Real_Time("cases", day_data, day_max) 

# proportion of reported cases / deaths per group ----------------------------#
plot_Real_GroupProp()
  # note: every distribution adds up to one, so the cols height indicate
  # how many of all cases occurred in this age group / gender


# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#
# simulated vs. real data ####
# ----------------------------------------------------------------------------#

# # Plot the parameter posteriors and overlaying the chains to check for -----#
# consistence ----------------------------------------------------------------#
{
  # Gather all parameters in theta
  pars <- c("beta", "epsilon","rho","pi","psi", "eta")
  subtitle = f(paste0("For {controls[chains]} chains and ",
               "{controls[iterations]} iterations per chain."))
  title = f(paste0("Posterior Density Plots ({controls[region]})" ))
  plot <- stan_dens(samples, pars = pars, separate_chains = T, nrow = 3) +
    labs(title = title, subtitle = subtitle) +
    scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
    coord_cartesian(xlim =c(0,1))
  if (controls[["savePlots"]]) {
    save_gg(plot = plot, name = "Posterior Density")
  }
  remove(pars, subtitle, title)
  plot
}

# Plot simulated cases (all, symptomatic, reported) vs real cases ------------#
plot_SimVsReal_Time("deaths", day_data = day_data, day_max = day_max)
plot_SimVsReal_Time("cases", day_data = day_data, day_max = day_max)

plot_SimVsReal_Group(metric = "cases")
plot_SimVsReal_Group(metric = "deaths")

plot_SimVsReal_Total("cases")
plot_SimVsReal_Total("deaths")

# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# parameter values ####
# ----------------------------------------------------------------------------#

# plot ascertainment rate per group ------------------------------------------#
plot_ascertainment("#CC333F")

# plot parameter traces ------------------------------------------------------#
stan_trace(samples, pars=c("beta", "epsilon","rho","pi","psi", "eta"))

# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# fatality ratios ####
# ----------------------------------------------------------------------------#

# plot CFR, sCFR and IFR -----------------------------------------------------#
# definition time: CFR is calculated as deaths / cases (per group), and can be
# calculated from both the simulated data and the real data



# ----------------------------------------------------------------------------#