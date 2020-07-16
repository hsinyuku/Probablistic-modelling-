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
source("setup.R")
source(paste0("R/01_DataManagement_", region, ".R"))
source("R/02_PrepareModel.R", echo = T)
source("R/04b_ModelDiagnostics_Plotting.R") # for visualizations
theme_set(theme_bw())

# Load the saved posterior samples in the data. I don't want to source
# 03_RunModel file here as it would force the whole sampling process to repeat
# every time we call this script. 
samples <-  readRDS(file = paste0("Posteriors/",region, "_Samples.Rds"))
# ----------------------------------------------------------------------------#

# Gather all paremeters in theta
pars <- c("beta", "epsilon","rho","pi","psi", "eta")


# Function to save plots
save_gg <- function(plot, name, region, width = 10, height = 6){
  ggsave(paste0("Figures/", region, " - ", name, ".png"), units = "in",
         width = width, height = height)
  # warning(paste0("Saved plot to ", paste0("Figures/", region, " - ", name, ".png")))
}

# Plot the parameter posteriors and overlaying the chains to check for
# consistence
stan_dens(samples, pars = pars, separate_chains = T, nrow = 3, ) +
  labs(title = paste0("Posterior Density Plots (",region,")"), 
       subtitle = ("For 4 separating chains, 1000 iterations / chain")) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) %>% 
  save_gg(plot = ., name = "Posterior Density", region, width = 10, height = 3.5)


# Plot simulated cases (all, symptomatic, reported) vs real cases
plot_incidence_cases(samples = samples,
                     data_list = data_list_model, # sourced from 02_
                     start_date = day_data,
                     end_date = day_max,
                     region = region) %>% 
  save_gg(plot = ., name = "Incidence Cases Timeline", region, width = 6, height = 3)

# Plot simulated deaths & residual deaths vs real deaths
plot_incidence_deaths(samples = samples,
                      data_list = data_list_model, # sourced from 02_
                      start_date = day_data,
                      end_date = day_max) %>% 
  save_gg(plot = ., name = "Incidence Deaths Timeline", region, width = 6, height = 3)

# Plot age distribution in % of real reported cases and sim reported symp. cases
plot_agedist_cases_perc(samples = samples,
                     data_list = data_list_model, #sourced from 02_
                     region = region) %>% 
  save_gg(plot = ., name = "Age Dist Cases", region, width = 5, height = 3)

# Plot age distribution in % of real reported deaths and sim deaths until tmax
plot_agedist_deaths_perc(samples = samples,
                        data_list = data_list_model, #sourced from 02_
                        region = region) %>% 
  save_gg(plot = ., name = "Age Dist Deaths", region, height = 3, width = 5)

# Plot ascertainment rate among age groups
plot_ascertainment(samples = samples,
                   data_list = data_list_model, # sourced from 02_
                   region = region) %>% 
  save_gg(plot = ., name = "Ascertainment Rate", region, height = 3, width = 5)

# Plot eta for each age groups (extension)
plot_eta(samples = samples,
         data_list = data_list_model, #sourced from 02_
         region = region) %>% 
  save_gg(plot = ., name = "Transmissibility Reduction", region, height = 3, width = 5)

# Take out sCFR by age groups, write it into Posteriors folder
icfr_by_age <- data.frame(ageGroup = seq(1,9,1),
                              A = summary(samples, "cfr_A_symptomatic_by_age")$summary %>% as.data.frame() %>% pull(`50%`),
                              B = summary(samples, "cfr_B_symptomatic_by_age")$summary %>% as.data.frame() %>% pull(`50%`),
                              C = summary(samples, "cfr_C_symptomatic_by_age")$summary %>% as.data.frame() %>% pull(`50%`),
                              D = summary(samples, "cfr_D_symptomatic_by_age")$summary %>% as.data.frame() %>% pull(`50%`)) %>% 
  round(3)

write.csv2(icfr_by_age, paste0("Posteriors/",region, " - iCFR.csv"))
