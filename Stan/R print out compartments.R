source("R/00_ContactMatrix_Gender_Age_Function.R")
source("R/01_DataManagement_Spain.R")


switch_eta <- function(t, t1, eta, nu, xi) {
  return(eta+(1-eta)/(1+exp(xi*(t-t1-nu))));
}

# Set parameters from paper, for Spain
tswitch = 8
S = 46
K = 9

eta = 0.254
nu = 0.2
xi = 1.19
pi = 0.001895
beta = 0.24
psi = 0.81

gt = 5.2      # generation time in days
q_P = 0.46    # contribution of presymptomatic infections to the transmission
tau_2 = 1/2.3 # days of incubation without transmission
tau_1 = 1/2.7 # days of incubation with reduced transmission

mu = (1-q_P)/(gt-1/tau_1-1/tau_2)
kappa = (q_P*tau_2*psi)/((1-q_P)*mu-(1-psi)*q_P*tau_2)

contact <- contact_matrix_gender_age(region = "Spain", type = "age")

# Print the reduction in transmissibilty
eta_result <- c(rep(NA, S))
for(t in 1:S){
  eta_result[t] <- switch_eta(t, tswitch, eta, nu, xi)
}

# Initial compartments (w)
y <- matrix(nrow = S, ncol = K*6)

for (k in 1:K){
  y[1, k] = (1-pi) * age_dist[k];
  y[1, K+k] = pi * age_dist[k];
  y[1, 2*K+k] = 0;
  y[1, 3*K+k] = 0;
  y[1, 4*K+k] = 0;
  y[1, 5*K+k] = 0;
}

# Loop through the timeline
t = 1
k = 1

f_inf <- c(rep(NA, 9))
f_inf_matrix <- matrix(nrow = S, ncol = K)

# Discrete Time Solver
for (t in 1:(S-1)){
  f_inf <- c(rep(NA, 9))
  # Total number of infectious people (time dependent)
  p_tswitch = switch_eta(t,tswitch,eta,nu,xi)
  
  # print(paste0("P_tswitch = ", p_tswitch))

  # Force of infection by age classes (time dependent): 
  # beta * p_tswitch * sum((number of infected people by age + 
  #                          + kappa*number of preclinical by age + kappa*number of asympto) / 
  #                        (total number of people by age) * (number of contact by age))
  
  
  for (k in 1:K) {
    
    f_inf_matrix[t,k] = beta * p_tswitch * sum((as.vector(y[t, (3*K+1):(4*K)]) + 
                                         kappa*as.vector(y[t, (2*K+1):(3*K)]) + 
                                         kappa*as.vector(y[t, (4*K+1):(5*K)])) / as.vector(age_dist) *
                                        as.vector(contact[(K*(k-1)+1):(k*K)]))
    f_inf[k] <- f_inf_matrix[t,k]
  }

  # print(paste0("f_inf = ",  f_inf))
  
  # Compartments
  for (k in 1:K) {
    # S: susceptible
    y[t+1, k] = (y[t, k]) - f_inf[k] * (y[t, k])
    # E: incubating (not yet infectious)
    y[t+1, K+k] = (y[t, K+k]) + f_inf[k] * (y[t, k]) - tau_1 * (y[t, K+k])
    # P: presymptomatic (incubating and infectious)
    y[t+1, 2*K+k] = (y[t, 2*K+k]) + tau_1 * (y[t, K+k]) - tau_2 * y[t, 2*K+k]
    # I: symptomatic
    y[t+1, 3*K+k] = (y[t, 3*K+k]) + psi * tau_2 * y[t, 2*K+k] - mu * y[t, 3*K+k]
    # A: asymptomatic
    y[t+1, 4*K+k] = (y[t, 4*K+k]) + (1-psi) * tau_2 * y[t, 2*K+k] - mu * y[t, 4*K+k]
    # C: cumulative number of infections by date of disease onset
    y[t+1, 5*K+k] = (y[t, 5*K+k]) + psi * tau_2 * y[t, 2*K+k]
  }
}
