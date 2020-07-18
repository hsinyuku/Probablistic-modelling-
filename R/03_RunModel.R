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
remove(list = ls())
source("setup.R")   # contains all other parameters
# the following parameters all determine which parts of the code are executed
# (or not). 
# Data for which region should be simulated and/or fitted?
region = "Spain"
if(region == "Baden-Württemberg") region = "BadenW"
if (!(region %in% regions)) warning(
  "The region you specified is not a correct string.\nFunctions will not ",
  "work! Please change the string. \nCheck the regions-object for ",
  "the correct spelling of the regions.")
# Should the data be differentiated acording to age groups or gender? Takes
# either "age" or "gender"
type = "age"
# Should the original data be plotted? Boolean.
visualise = TRUE
# Should the Stan-model include the updating of the posteriors? If not, the
# posteriors will not be fitted to the data! Takes on 1 (yes, should be fitted)
# or 0 (no, should not be fitted).
inference = 1
if (class(inference) == "logical") inference = as.integer(inference)
# Should some information be printed for debugging?
doprint = 0
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#
# Cleaning the global environment so no data from other runs of the code can 
# influence the current run.
remove(list = ls()[!(ls() %in% list("region", "type", "visualise", "inference",
                                    "doprint"))]) 
  # clearing the work space, except for the necessary control parameters
source(paste0("R/01_DataManagement_", region, ".R"))
source("R/02_PrepareModel.R", echo = T)
# Works with:
#   Austria
#   Spain
#   Baden-Württemberg
#   Bavaria
#   Hubei
#   Switzerland
#   Lombardy
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# preparing and running the model ####
# ----------------------------------------------------------------------------#
model_DSO = stan_model("Stan/all_regions_Stan_model.stan")
beepr::beep()

# Sampling from the posterior distribution
samples = sampling(model_DSO, data = data_list_model, iter = 400,
                       chains = 1, init= 0.5,
                       control=list(max_treedepth=10,adapt_delta=0.8))

# Save the samples and the DSO object to RDS
saveRDS(object = model_DSO, file = paste0("Posteriors/", region, "_DSO.Rds"))
saveRDS(object = samples, file = paste0("Posteriors/", region, "_samples.Rds"))


compartment_data <- summary(samples, pars = "compartment_data")$summary
row.names(compartment_data) <- c()
compartment_data <- compartment_data %>% as.data.frame() %>%
  cbind(
    date = seq(day_data, day_max, 1),
    ageGroup = as.integer(rep(1:9, 46 * 6)),
    compartment = rep(c(
      rep("S", 9),
      rep("E", 9),
      rep("P", 9),
      rep("I", 9),
      rep("A", 9),
      rep("C", 9)
    ), 46),
    .
  ) %>%
  arrange(date)

compartment_data %>% group_by(date) %>%
  summarise(mean = sum(mean), median = sum(`50%`)) %>% View()

summary(samples, pars = "predicted_total_overall_deaths_tmax_by_age")$summary

# This maybe is a way to load y from data
y = rstan::extract(samples,"y")[[1]]

