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
source("R/04b_ModelDiagnostics_Plotting.R") # for visualizations
theme_set(theme_bw())

# Load the saved posterior samples in the data. I don't want to source
# 03_RunModel file here as it would force the whole sampling process to repeat
# every time we call this script. 
samples <-  readRDS(file = paste0("Posteriors/",region, "_samples1000.Rds"))
# ----------------------------------------------------------------------------#

# Gather all paremeters in theta
pars <- c("beta", "epsilon","rho","pi","psi", "eta")


# Function to save plots
save_gg <- function(plot, name, region){
  ggsave(paste0("Figures/", region, " - ", name, ".png"), units = "in", width = 10, height = 6)
}




# Plot the parameter posteriors and overlaying the chains to check for
# consistence
stan_dens(samples, pars = pars, separate_chains = T) +
  labs(title = paste0("Posterior Density Plots (",region,")"), subtitle = ("For 4 separating chains")) %>% 
  save_gg(plot = ., name = "Den", region)

print(samples, pars = pars)

# Plot simulated cases (all, symptomatic, reported) vs real cases
plot_incidence_cases(samples = samples,
                     data_list = data_list_model, # sourced from 02_
                     start_date = day_data,
                     end_date = day_max,
                     region = region) %>% 
  save_gg(plot = ., name = "Incidence Cases Timeline", region)


plot_incidence_deaths(samples = samples,
                     data_list = data_list_model, # sourced from 02_
                     start_date = day_data,
                     end_date = day_max) %>% 
  save_gg(plot = ., name = "Incidence Deaths Timeline", region)


plot_agedist_cases_perc(samples = samples,
                     data_list = data_list_model, #sourced from 02_
                     region = region) %>% 
  save_gg(plot = ., name = "Age Dist Cases", region)

plot_agedist_deaths_perc(samples = samples,
                        data_list = data_list_model, #sourced from 02_
                        region = region) %>% 
  save_gg(plot = ., name = "Age Dist Deaths", region)


# Plot ascertainment rate among age groups
plot_ascertainment(samples = samples,
                   data_list = data_list_model, # sourced from 02_
                   region = region) %>% 
  save_gg(plot = ., name = "Ascertainment Rate", region)




summary(samples, "eta")$summary

plot_total_cases(samples = samples,
                     data_list = data_list_model) # sourced from 02_

