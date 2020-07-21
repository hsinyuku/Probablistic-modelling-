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
  source("setup.R")
  init_controls(list(region = "Spain"))
  source(paste0("R/01_DataManagement_", controls["region"], ".R"))
  source(paste0("R/02_PrepareModel_", controls["type"], ".R"))
  source("R/04b_ModelDiagnostics_Functions.R") # for visualizations
}
#theme_set(theme_bw())

# Load the saved posterior samples in the data (obviously without sampling
# them every time).
samples <-  readRDS(file = paste0("Posteriors/Spain_Samples_200718.Rds"))

# Gather all parameters in theta
pars <- c("beta", "epsilon","rho","pi","psi", "eta")
save_plots = F
region <- controls["region"] # easier access to that variable
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# visual inspection of sampling ####
# ----------------------------------------------------------------------------#
# Plot the parameter posteriors and overlaying the chains to check for
# consistence
stan_dens(samples, pars = pars, separate_chains = T, nrow = 3) +
  labs(title = paste0("Posterior Density Plots (",region,")"), 
       subtitle = ("For 4 separating chains, 1000 iterations / chain")) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) %>% 
  save_gg(plot = ., name = "Posterior Density", region)
# ----------------------------------------------------------------------------#




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


summary(samples, "compartment.data")$summary
