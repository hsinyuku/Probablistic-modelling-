# ----------------------------------------------------------------------------#
# RunModel_Template.R
# 
# This idea of this file is to only run the model. The computation of variables
# should happen in a separate file that has to be called before.
# !!! Currently, the name of the contact matrix still has to be changed!!!
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#
# this line is an attempt at making the code execution stop when there are 
# errors in on of the files to be sourced
source("R/02_PrepareModel_Template.R")   # contains all other parameters
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# preparing and running the model ####
# ----------------------------------------------------------------------------#
model16DSO = stan_model("Stan/model16.stan")

