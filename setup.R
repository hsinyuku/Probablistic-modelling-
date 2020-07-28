# ----------------------------------------------------------------------------#
# setup.R
# 
# This file exclusively contains the necessary packages, and functions that are 
# not very specific to just one package.
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# packages ####
# ----------------------------------------------------------------------------#
# Please load all packages here! That way, we can avoid loading anything 
# multiple times. If possible, please restrict your use of packages, since
# every package slows down R a little. If you need only one function, consider
# using namespaces (::).#
library(tidyverse)
library(lubridate)
library(rstan)
rstan_options(auto_write = TRUE)
library(tictoc) # to compare some runtimes
library(socialmixr)
library(data.table) # for contact matrix
# library(remotes)
# remotes::install_github("GuangchuangYu/nCov2019", dependencies = T, force = T)
  # installation instructions can be found here:
  # https://guangchuangyu.github.io/nCov2019/
  # this currently does not work (Lukas)
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# functions ####
# ----------------------------------------------------------------------------#

# function to load data into R -----------------------------------------------#
# this function will check on whether a dataset is a) already loaded; if not, 
# b) whether there exists a .Rds file that contains the same data. Only if this
# is not the case, will the function c) load the data from a non-binary file 
# format and save it as an .Rds. This way, we can reduce the loading time, 
# especially of big datasets, while also avoiding to overwriting existing data.

# functions used in prepare_model --------------------------------------------#
# transforming original Linton parameters to parameters of the lognormal 
# distribution. Reason: the values that we know from Linton for mu and sigma
# of the log-normal distribution are not actually the two parameters of that
# distribution, but the expected value and variance of the resulting 
# probability density. Mu and sigma as parameters of the lognormal distribution
# as R wants them can be calculated from the expected value and variance as
# described in the English Wikipedia entry (section 1.1)
# (https://en.wikipedia.org/wiki/Log-normal_distribution)
get_par_lnorm = function(m, s) {
  mu    = log(m) - 1/2 * log((s/m)^2+1)
  sigma = sqrt(log((s/m)^2+1))
  return(list(mu=mu, sigma=sigma))
}

# function to delete all objects in the global environment, with exceptions---#
remove_except <- function(element_list) {
  # element_list must be a list of strings!
  objects <- ls(name = globalenv())
  indices <- objects %in% element_list
  delete_objects <- objects[!indices]
  keep_objects <- objects[indices]
  remove(list = delete_objects, pos = globalenv())
  print("Removed the following objects:")
  print(delete_objects)
}

# function to check the controls, i.e. the objects that control the flow of --#
# the code (which should be run) ---------------------------------------------#
check_controls <- function(controls) {
  # List all possible regions to compare possible inputs at later points, so they
  # can't be spelled wrong or anything
  regions = list("Austria", "BadenW", "Bavaria", "Hubei", "Lombardy",
                 "Switzerland", "Spain")
  # Checking region; accessing the object directly in the global environment,
  # because otherwise the if-else checks on the local object region, which
  # is not changed by the first line
  if(controls["region"] == "Baden-Wuerttemberg") {
    controls$region <- "BadenW"
  }
  if (!(controls$region %in% regions)) {
   print(controls$region)
   warning("The region you specified is not a correct string. Functions will ",
           "not work! Please change the string. Check the regions-object for ",
           "the correct spelling of the regions.")
   return(FALSE)
  }
  # Checking type: Age or Gender?
  type <<- stringr::str_to_title(controls$type)
    # <<- will assign type in the global environment
  # inference must be integer, not a boolean
  if (class(controls$inference) == "logical") {
    controls$inference <- as.integer(controls$inference)
  } 
  if (!(controls$inference %in% c(0, 1))) {
    warning("Inference must be either 1 or 0!")
    return(FALSE)
  } 
  # ind_eta: should the model with varying eta be run?
  if (controls$ind_eta == F | controls$ind_eta == "CommonEta") {
    controls$ind_eta <- "CommonEta"
    }  else {
      controls$ind_eta <- "VaryingEta"
  }
    
  # use_cores: how many cores should be used?
  if (controls$use_cores == "all")  {
    controls$use_cores <- parallel::detectCores()
  } else if (as.integer(controls$use_cores) > parallel::detectCores()) {
    warning("You don't have that many cores! Change use_cores:")
    return(FALSE)
  }
  else controls$use_cores <- as.integer(controls$use_cores)
  options(mc.cores = parallel::detectCores())
  return(controls)
}
  

# this function generates a controls-object from a given posterior name
extractPosteriorMetadata <- function(posteriorName, visualise = F, savePlots = F) {
  posteriorControls <- str_split(
    str_sub(posteriorName, 1, str_locate(posteriorName, "\\.")[1]-1), "_")[[1]]
  # specify here which posterior file you want to load!
  return(list(region = posteriorControls[1],
                     type = posteriorControls[2],
                     ind_eta = posteriorControls[3],
                     chains = posteriorControls[6],
                     iterations = posteriorControls[4],
                     visualise = visualise,
                     savePlots = savePlots,
                     timestamp = posteriorControls[8],
                     # these controls don't do anything, but need to have some value
                     inference = 1,
                     use_cores = 1))
}


# get a nice table with all the parameters; also include some metadata abot
# the parameters: do they refer to groups, do they refer to days, do they
# include the delay
quantityMetadataTable <- function(group) {
  group <- str_to_lower(group)
  names <- c("parameter", "per_day", "per_group", "with_delay")
  x <- list(c("predicted_reported_incidence_symptomatic_cases", T, F, NA),
            c("predicted_overall_incidence_symptomatic_cases", T, F, NA),
            c("predicted_overall_incidence_all_cases", T, F, NA),
            c("predicted_reported_incidence_deaths", T, F, T),
            c("predicted_overall_incidence_deaths", T, F, T),
            # "compartment_data",
            c("predicted_comp_reported_diffC", T, T, NA),
            c("predicted_comp_overall_diffC", T, T, NA),
            c("predicted_comp_overall_diffA", T, T, NA),
            c("predicted_comp_diffM", T, T, T),
            c(paste0("predicted_total_reported_symptomatic_cases_by_", group), F, T, NA),
            c(paste0("predicted_total_overall_symptomatic_cases_by_", group), F, T, NA),
            c(paste0("predicted_total_overall_all_cases_by_", group), F, T, NA),
            c(paste0("predicted_total_overall_deaths_tmax_by_", group), F, T, F),
            c(paste0("predicted_total_overall_deaths_delay_by_", group), F, T, T),
            c("predicted_total_reported_symptomatic_cases", F, F, NA),
            c("predicted_total_overall_symptomatic_cases", F, F, NA),
            c("predicted_total_overall_all_cases", F, F, NA),
            c("predicted_total_overall_deaths_tmax", F, F, F),
            c("predicted_total_overall_deaths_delay", F, F, T),
            c(paste0("cfr_A_symptomatic_by_", group), F, T, F),
            c(paste0("cfr_B_symptomatic_by_", group), F, T, T),
            c(paste0("cfr_C_symptomatic_by_", group), F, T, F),
            c(paste0("cfr_D_symptomatic_by_", group), F, T, T),
            c(paste0("cfr_C_all_by_", group), F, T, F),
            c(paste0("cfr_D_all_by_", group), F, T, T),
            c("cfr_A_symptomatic", F, F, T),
            c("cfr_B_symptomatic", F, F, T),
            c("cfr_C_symptomatic", F, F, F),
            c("cfr_D_symptomatic", F, F, T),
            c("cfr_C_all", F, F, F),
            c("cfr_D_all", F, F, T))
  parameters <- tibble(x) %>% mutate(x = map(x, function(x) {
    x <- as.data.frame(x)
    rownames(x) <- names
    return(as_tibble(t(x)))
  })) %>% unnest(x)
  return(parameters)
}

# function that extracts data and metadata from a posterior, given the
# posterior's name. Type must be given as well.
initialiseSample <- function(posteriorName, type) {
  # Load the saved posterior samples in the data (obviously without sampling
  # them every time).
  postPath <- paste0("Posteriors/", posteriorName)
  if(!file.exists(postPath)) {
    stop(paste0("The posterior you chose does not exist (on your machine? ",
                "on this branch?). Remember that you have to download the ",
                "posteriors manually from GDrive!"))
  }
  samples <- readRDS(file = postPath)
  parameters <- quantityMetadataTable(type)
  # first get those parameters that are per day:
  parameters_per_day <- filter(parameters, per_day == T & per_group == F) %>% 
    pull(parameter)
  # then get data about those parameters
  parameters_per_day <- summary(samples, parameters_per_day)$summary %>% 
    as_tibble(rownames = "parameter") %>% 
    mutate(index = str_extract(parameter, "[:digit:]+"),
           parameter = str_extract(parameter, "[_[:alpha:]]*(?=\\[)")) %>% 
    select(parameter, index, mean:Rhat)
  
  # get those parameters that are per group:
  parameters_per_group <- filter(parameters, per_day == F & per_group == T) %>% 
    pull(parameter)
  # then get data about those parameters
  parameters_per_group <- summary(samples, parameters_per_group)$summary %>% 
    as_tibble(rownames = "parameter") %>% 
    mutate(index = str_extract(parameter, "[:digit:]+"),
           parameter = str_extract(parameter, "[_[:alpha:]]*(?=\\[)")) %>% 
    select(parameter, index, mean:Rhat)
  
  # get those parameters that are per day and group:
  parameters_per_both <- filter(parameters, per_day == T & per_group == T) %>% 
    pull(parameter)
  # then get data about those parameters
  parameters_per_both <- summary(samples, parameters_per_both)$summary %>% 
    as_tibble(rownames = "parameter") %>% 
    mutate(index = str_extract_all(parameter, "[:digit:]+"),
           parameter = str_extract(parameter, "[_[:alpha:]]*(?=\\[)"),
           rowindex = map_int(index, function(x) as.integer(x[[1]])),
           colindex = map_int(index, function(x) as.integer(x[[2]]))) %>% 
    select(parameter, rowindex, colindex, mean:Rhat)
  
  # get those parameters that are total values:
  parameters_per_none <- filter(parameters, per_day == F & per_group == F) %>% 
    pull(parameter)
  # then get data about those parameters
  parameters_per_none <- summary(samples, parameters_per_none)$summary %>% 
    as_tibble(rownames = "parameter") %>% 
    select(parameter, mean:Rhat)
  
  # get metadata about the posterior
  sampler_params <- get_sampler_params(samples, inc_warmup = TRUE)
  # sampler_params returns a list with one entry per chain
  chains <- length(sampler_params)
  # length of the list is number of chains
  iterations <- nrow(sampler_params[[1]])
  # each row in each element of the list is one iteration without warmup
  warmup <- iterations - nrow(sampler_params[[1]])
  return(list(
    "sample" = samples,
    "parameters" = list(
      "per_day" = parameters_per_day, "per_group" = parameters_per_group,
      "per_both" = parameters_per_both,"per_none" = parameters_per_none
    ),
    "metadata" = list("chains" = chains, "iterations" = iterations,
                      "warmup" = warmup)
  ))
}

# ----------------------------------------------------------------------------#
