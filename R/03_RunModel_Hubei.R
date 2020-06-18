# ----------------------------------------------------------------------------#
# PrepareModel_Hubei.R
# 
# This file uses all parameters to run the model.
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#
# this line is an attempt at making the code execution stop when there are 
# errors in on of the files to be sourced
source("setup.R")
source("R/01_DataManagement_Hubei.R") # contains data
source("R/02_PrepareModel_Hubei.R")   # contains all other parameters
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# transmission parameters ####
# ----------------------------------------------------------------------------#
# still trying to find out what those actually mean!
t0      = 0
S       = as.numeric(day_max-day_start)
t_data  = as.numeric(day_data-day_start)
ts      = t_data:S
D       = as.numeric(day_max - day_data+1)
tswitch = as.numeric(day_quarantine-day_start)
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# specifying data for Stan ####
# ----------------------------------------------------------------------------#
data_list_model16A = {list(
  # Structure ------------------------------#
  K        = 9,        # number of age groups
  age_dist = age_dist, # age distribution
  pop_t    = pop_t,    # total population
  t0       = 0,        # start time in days
  # Controls -------------------------------#
  t_data    = t_data,
  tswitch   = tswitch,
  S         = S,
  ts        = ts,
  inference = 0,
  doprint   = 0,
  # Data to fit ----------------------------#
  D                = D,
  incidence_cases  = incidence_cases,  # cases per day
  incidence_deaths = incidence_deaths, # deaths per day
  agedistr_cases   = cases_tmax,       # age distribution of cases
  agedistr_deaths  = mort_tmax,        # age distribution of deaths
  # Parameters for Prior Distributions -----#
  p_beta    = p_beta,
  p_eta     = p_eta,
  p_pi      = p_pi,
  p_psi     = p_psi,
  p_epsilon = p_epsilon,
  p_phi     = p_phi,
  p_rho     = p_rho,
  p_xi      = p_xi,
  p_nu      = p_nu,
  # Fixed parameters -----------------------#
  contact=contact_matrix_china,
  p_q_P             = q_P,
  p_incubation      = incubation,
  p_preclinical     = 1/tau_2,
  p_generation_time = gt,
  p_children_trans  = 1,
  # Fixed corrections ----------------------#
  p_report_80plus      = 1,
  p_underreport_deaths = 1,
  p_underreport_cases  = 1,
  # Fixed delays ---------------------------#
  G       = 60,
  p_gamma = gamma
)}
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# preparing and running the model ####
# ----------------------------------------------------------------------------#
M_model16 = stan_model("models/model16.stan")