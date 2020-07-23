# ----------------------------------------------------------------------------#
# ModelDiagnostics.R
# 
# This idea of this file is to run model diagnostics. Functions for this file
# come from the script 04a_ModelDiagnostics_Plotting_Functions.
# This script should be able to run on its own. Therefore, we need to provide
# certain control variables.
# ----------------------------------------------------------------------------#

region = "Spain"

type = "Age"

# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#
{
  source("setup.R")
  source(paste0("R/01_DataManagement_", region, ".R"))
  source(paste0("R/02_PrepareModel_", type, ".R"), echo = T)
  source("R/04b_ModelDiagnostics_Functions.R") # for visualizations
  theme_set(theme_bw())
}
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# Defining control variables ####
# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#

# Load the saved posterior samples in the data. I don't want to source
# 03_RunModel file here as it would force the whole sampling process to repeat
# every time we call this script. 
samples <-  readRDS(file = paste0("Posteriors/",region, "_Samples_200718.Rds"))
# ----------------------------------------------------------------------------#

# Gather all paremeters in theta
pars <- c("beta", "epsilon","rho","pi","psi", "eta")


# Plot the parameter posteriors and overlaying the chains to check for
# consistence
stan_dens(samples, pars = pars, separate_chains = T, nrow = 3) +
  labs(title = paste0("Posterior Density Plots (",region,")"), 
       subtitle = ("For 4 separating chains, 1000 iterations / chain")) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

# Plot simulated cases (all, symptomatic, reported) vs real cases
plot_incidence_cases(samples = samples,
                     data_list = data_list_model, # sourced from 02_
                     start_date = day_data,
                     end_date = day_max,
                     region = "Spain") 

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


# Analysis of the compartments ----------------------------------------------- #

summary(samples, "compartment.data")$summary

# Take out the daily values of the compartments to visualize SEPIAR model
# Content-wise not successfully
compartment_data <- summary(samples, pars = "compartment_data")$summary
row.names(compartment_data) <- c()
compartment_data <- compartment_data %>% as.data.frame() %>%
  cbind(
    date = rep(seq(day_data, day_max, 1), each= 6*K),
    ageGroup = as.integer(rep(1:9, 46 * 6)),
    compartment = rep(c(
      rep("S", 9),
      rep("E", 9),
      rep("P", 9),
      rep("I", 9),
      rep("A", 9),
      rep("C", 9)
    ), 46),
    compartmentNo = rep(c(
      rep(1, 9),
      rep(2, 9),
      rep(3, 9),
      rep(4, 9),
      rep(5, 9),
      rep(6, 9)
    ), 46),
    .
  ) %>%
  arrange(date, compartmentNo, ageGroup)

compartment_data %>% group_by(date) %>%
  summarise(mean = sum(mean), median = sum(`50%`)) %>% View()

# Check compartment S throughout the time line
compartment_data %>% filter(compartment == "S") %>% 
  group_by(date) %>% 
  summarise(mean = sum(mean)) %>% 
  plot(mean, type = "l")

# Check compartment E throughout the time line
compartment_data %>% filter(compartment == "E") %>% 
  group_by(date) %>% 
  summarise(mean = sum(mean)) %>% 
  plot(mean, type = "l")

# Look at other compartments on t = 1
compartment_data %>% filter(date == "2020-03-02", !(compartment %in% c("S","E")))

# ---------------------------------------------------------------------------- #