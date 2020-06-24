# ----------------------------------------------------------------------------#
# RunModel_Spain.R
# 
# This idea of this file is to only run the model. The computation of variables
# should happen in a separate file that has to be called before.
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#
# this line is an attempt at making the code execution stop when there are 
# errors in on of the files to be sourced
source("R/02_PrepareModel_Spain.R")   # contains all other parameters
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# specifying data for Stan ####
# ----------------------------------------------------------------------------#
data_list_model16A = {list(
  # Structure ------------------------------#
  K        = K,        # number of age groups
  age_dist = age_dist, # age distribution
  pop_t    = pop_t,    # total population
  t0       = t0,       # start time in days
  # Controls -------------------------------#
  t_data    = t_data,
  tswitch   = tswitch,
  S         = S,
  ts        = ts,
  inference = inference,
  doprint   = doprint,
  D         = D,
  # Data to fit ----------------------------#
  incidence_cases  = incidence_cases,  # cases per day
  # incidence_deaths = incidence_deaths, # deaths per day
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
  contact           = contact_matrix_europe,
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
  G       = G,
  p_gamma = gamma
)}
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# preparing and running the model ####
# ----------------------------------------------------------------------------#
{
  # tictoc::tic()
  M_model16 = stan_model(model_code = "Stan/model16.stan")
  # tictoc::toc()
}
T_model16 = sampling(M_model16,data = data_list_model16A,iter = 1000,chains = 10,
                     init=0.5, control=list(max_treedepth=10,adapt_delta=0.8))

print(T_model16,pars=c("beta","eta","epsilon","rho","pi","psi"))


D_S_model16ASP = read_rdump("posterior_samples/data_S_model16SP-A_2020-05-04-16-26-01.R")

S_model16ASP = read_stan_csv(paste0("posterior_samples/",
                                    dir("posterior_samples",
                                        pattern = 'S_model16SP-A_2020-05-04-16-26-01_[[:digit:]]+.csv')))

# Checks
print(S_model16ASP,pars=c("beta","eta","epsilon","rho","pi","psi"),digits_summary=4)