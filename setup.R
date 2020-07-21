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
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# regions ####
# ----------------------------------------------------------------------------#
# List all possible regions to compare possible inputs at later points, so they
# can't be spelled wrong or anything
regions = list("Austria", "BadenW", "Bavaria", "Hubei", "Lombardy",
               "Switzerland", "Spain")
# ----------------------------------------------------------------------------#