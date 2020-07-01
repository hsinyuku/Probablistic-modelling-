# ----------------------------------------------------------------------------#
# RunModel_Spain.R
# 
# This idea of this file is to only run the model. The computation of variables
# should happen in a separate file that has to be called before.
# ----------------------------------------------------------------------------#
# 
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
spain_samples = sampling(spain_DSO,data = data_list_model,iter = 10,chains = 2,
                     init= 0.5, control=list(max_treedepth=10,adapt_delta=0.8))

# Save the samples and the DSO object to RDS
saveRDS(object = spain_DSO, file = "Posteriors/spain_DSO.Rds")
saveRDS(object = spain_samples, file = "Posteriors/spain_samples.Rds")



compartment_data <- summary(spain_samples, pars = "compartment_data")$summary
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

compartment_data %>% group_by(date) %>% summarise(mean = sum(mean), median = sum(`50%`)) %>% View()




summary(spain_samples, pars = "predicted_total_overall_deaths_tmax_by_age")$summary
