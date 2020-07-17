# ----------------------------------------------------------------------------#
# RunModel.R
# 
# This idea of this file is to only run the model. The computation of variables
# should happen in a separate file that has to be called before.
# ----------------------------------------------------------------------------#


{
# ----------------------------------------------------------------------------#
# preliminaries ####
# ----------------------------------------------------------------------------#
# this line is an attempt at making the code execution stop when there are 
# errors in on of the files to be sourced
remove(list = ls()) # clearing the work space
source("setup.R")   # contains all other parameters

region = "Lombardy"

# type of population distribution: age group or gender. Takes either "age" or 
# "gender"

type = "age"

visualise = F

if(region == "Baden-Württemberg") {
  region = "BadenW"
}
if (!(region %in% regions)) {
  warning("The region you specified is not a correct string.\nFunctions will not ",
          "work! Please change the string. \nCheck the regions-object for ",
          "the correct spelling of the regions.")
}
# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#

# this line is an attempt at making the code execution stop when there are 
# errors in on of the files to be sourced
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
}


# ----------------------------------------------------------------------------#
# preparing and running the model ####
# ----------------------------------------------------------------------------#
model16DSO = stan_model("Stan/all_regions_Stan_model.stan")

# Sampling from the posterior distribution
samples = sampling(model16DSO,data = data_list_model, iter = 1000,
                       chains = 4, init= 0.5,
                       control=list(max_treedepth=10,adapt_delta=0.8))

# Save the samples and the DSO object to RDS
saveRDS(object = model16DSO, file = paste0("Posteriors/", region, "_DSO.Rds"))
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

