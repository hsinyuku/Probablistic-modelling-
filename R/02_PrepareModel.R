# ----------------------------------------------------------------------------#
# PrepareModel.R
# 
# This file does computations on some preliminary variables (mainly fixed
# parameters) and prepares them for Stan. It should not be called on its own.
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# distribution of time from sympton onset to death (I) ####
# ----------------------------------------------------------------------------#
# Fom Linton et al - these values are hardwired
linton_mean = 20.2
linton_sd   = 11.6

# the given values are mean and standard deviation of the lognormal-
# distribution and thus have to be transformed (we guess)
linton_pars = get_par_lnorm(linton_mean,linton_sd)

# Discretize - I vectorised this function, makes it a little bit faster (like
# 4 milliseconds)
gamma = plnorm((1:60) + 0.5,linton_pars$mu,linton_pars$sigma) - 
  plnorm((1:60) - 0.5,linton_pars$mu,linton_pars$sigma)
# Normalise gamma to an area of 1
gamma = gamma/sum(gamma)

# visualising I, discretised and real
if(visualise) {
  ggplot() +
    geom_col(data = tibble(x = 1:60, y = gamma),
             aes(x = x, y = y), fill = "grey", alpha = 0.5, col = "grey") +
    geom_line(data = tibble(x = seq(0, 60, 0.001)) %>% 
                mutate(y = dlnorm(x, linton_pars$mu,linton_pars$sigma)),
              aes(x = x, y = y), col = "black") +
    coord_cartesian(xlim = c(0, 60))
}

remove(linton_mean, linton_sd, linton_pars)
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# proportion of infected individuals that develop symptoms psi ####
# ----------------------------------------------------------------------------#
# uncertainty on symptomatic rate from a systematic review 
# (pooled mean: 19% (prediction interval 11 – 29%))
m    = 1-0.19
low  = 1-0.29
high = 1-0.11
v = (mean(c(m-low,high-m))/qnorm(0.975))^2

# this is probably some method to generate parameters for a beta distribution
# when you have a mean and a prediction interval  
p_psi_alpha = m*(m/v*(1-m)-1)
p_psi_beta = (1-m)/m*p_psi_alpha

remove(m, low, high, v)
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# transmission parameters ####
# ----------------------------------------------------------------------------#

gt = 5.2      # generation time in days
q_P = 0.46    # contribution of presymptomatic infections to the transmission
tau_2 = 1/2.3 # days of incubation without transmission
tau_1 = 1/2.7 # days of incubation with reduced transmission
mu = (1-q_P)/(gt-1/tau_1-1/tau_2) 
# duration for which symptomatic individuals are infectious
# # duration for which symptomatic individuals are infectious
psi = rbeta(1000, p_psi_alpha, p_psi_beta)
kappa = (q_P*tau_2*psi)/((1-q_P)*mu-(1-psi)*q_P*tau_2)
# reduction in transmissibility

# ----------------------------------------------------------------------------#
# Contact matrix ####
# ----------------------------------------------------------------------------#
# select the appropriate contact matrix with regard to region and type
source("R/00_ContactMatrix_Gender_Age_Function.R")
contact_matrix <- contact_matrix_gender_age(type = type, region = region)

# visualising the contact matrix
if(visualise) {
  tibble(contacts = contact_matrix) %>%
    mutate(age1 = rep(1:9,9), age2 = rep(1:9,each=9)) %>%
    ggplot() +
    geom_tile(aes(x=age2,y=age1,fill=contacts))
}
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# fixed corrections and delays ####
# ----------------------------------------------------------------------------#
# Fixed corrections ----------------------#
p_report_80plus      = 1 # this if 1 for every region
p_underreport_deaths = p_underreport_deaths
p_underreport_cases  = p_underreport_cases
p_children_trans     = 1 # this is 1 for every region
# Fixed delays ---------------------------#
G       = 60
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# so-called controls ####
# ----------------------------------------------------------------------------#
# still trying to find out what those actually mean!
t0        = 0
S         = as.numeric(day_max-day_start)
t_data    = as.numeric(day_data-day_start)
ts        = t_data:S
D         = as.numeric(day_max - day_data+1)
tswitch   = as.numeric(day_quarantine - day_start)
inference = 0
doprint   = 0
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# mising structure parameters ####
# ----------------------------------------------------------------------------#
t0 = 0
K  = 9 
# ----------------------------------------------------------------------------#


# other free parameters ####
# ----------------------------------------------------------------------------#
{
  p_beta    = c(1, 1)
  p_eta     = c(1, 1)
  p_pi      = c(1, 999)
  p_psi     = c(p_psi_alpha, p_psi_beta)
  p_epsilon = c(1,1)
  p_phi     = 1/100
  p_rho     = c(1, 1)
  p_xi      = c(1, 1)
  p_nu      = 1/5
}
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# specifying data for Stan ####
# ----------------------------------------------------------------------------#
data_list_model = {list(
  # Structure ------------------------------#
  K        = K,        # number of age groups
  age_dist = age_dist, # age distribution
  pop_t    = pop_t,    # total population
  t0       = t0,        # start time in days
  # Controls -------------------------------#
  t_data    = t_data,
  tswitch   = tswitch,
  S         = S,         # days between day_max and day_start
  ts        = ts,
  inference = inference,
  doprint   = doprint,
  D         = D,        # days between day_max and day_start, but +1 (not clear why)
  # Data to fit ----------------------------#
  incidence_cases  = incidence_cases,  # cases per day
  incidence_deaths = incidence_deaths, # deaths per day
  agedistr_cases   = agedistr_cases,   # age distribution of cases
  agedistr_deaths  = agedistr_deaths,  # age distribution of deaths
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
  contact           = contact_matrix,
  p_q_P             = q_P,
  p_incubation      = 1/tau_2 + 1/tau_1,
  p_preclinical     = 1/tau_2,
  p_generation_time = gt,
  p_children_trans  = p_children_trans,
  # Fixed corrections ----------------------#
  p_report_80plus      = p_report_80plus,
  p_underreport_deaths = p_underreport_deaths,
  p_underreport_cases  = p_underreport_cases,
  # Fixed delays ---------------------------#
  G       = G, # fixed delay
  p_gamma = gamma
)}