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
library(tictoc) # to compare some runtimes
library(socialmixr)
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

# other functions ------------------------------------------------------------#
# this function is used to avoid downloading data from online sources all the
# time. It checks whether the object to be downloaded is already in the global
# environment; if not, it checks whether there is a .Rds-object with the data
# to read into the global environment. If both is not the case, it downloads 
# the data, reads it into the global enviromment and saves the data as .Rds
check_online_data_existence = function(download_function(), object_name, file_name) {
  if (!file.exists(paste0("data/", file_name))) {
    object_name <- readRDS(file_name)
  }
  if (!exists(object_name)) {
    stop()
  }
  object_name <- download()
}

# ----------------------------------------------------------------------------#
