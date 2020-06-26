# ----------------------------------------------------------------------------#
# PrepareModel_Lombardy.R
# 
# This file does computations on some preliminary variables (mainly fixed
# parameters) and prepares them for Stan.
# ----------------------------------------------------------------------------#
# This is almost the same for Spain; it even has a hardwired contact matrix
# as well. Can we somehow remove this? Changes are in the correction parameters

# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#

source("R/01_DataManagement_Lombardy.R")

# set this to TRUE if you want visual inspection of paremeters
visualise = T 
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

# Discretize 
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
# (pooled mean: 19% (prediction interval 11 - 29%))
m    = 1-0.19
low  = 1-0.29
high = 1-0.11
v = (mean(c(m-low,high-m))/qnorm(0.975))^2

#  generate parameters for a beta distribution
 
p_psi_alpha = m*(m/v*(1-m)-1)
p_psi_beta = (1-m)/m*p_psi_alpha

remove(m, low, high, v)
# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#
# transmission parameters ####
# ----------------------------------------------------------------------------#
# reduced transmissibility of pre- and a-symptomatics kappa:
# He et al: 44% (25 -69%) of secondary cases were infected during the index 
# cases - presymptomatic stage http://nature.com/articles/s41591-020-0869-5
# Liu estimated 46% (21 - 46%) (S. Funk group) 
# https://wellcomeopenresearch.org/articles/5-58
# Ganyani (Wallinga en Hens) 48% (32-67%) 
# https://medrxiv.org/content/10.1101/2020.03.05.20031815v1

gt    = 5.2   # generation time in days
q_P   = 0.46  # contribution of presymptomatic infections to the transmission
incubation = 5 # Virus incubation time in days
tau_2 = 1/2.3 # days of incubation without transmission
tau_1 = 1/(incubation - 1/tau_2) # days of incubation with reduced transmission
mu    = (1-q_P)/(gt-1/tau_1-1/tau_2) 
              # duration for which symptomatic individuals are infectious
psi = rbeta(1000, p_psi_alpha, p_psi_beta)
kappa = (q_P*tau_2*psi)/((1-q_P)*mu-(1-psi)*q_P*tau_2)
              # reduction in transmissibility

# visualising kappa and psi
if(visualise) {
  tibble(kappa, psi) %>%
    pivot_longer(everything()) %>% 
    ggplot() +
    geom_density(aes(x = value, color = name))
}
remove(psi)

# contact matrix

contact_matrix_europe = c(5.13567073170732, 1.17274819632136, 0.982359525171638, 
                          2.21715890088845, 1.29666356906914, 0.828866413937242, 
                          0.528700773224482, 0.232116187961884, 0.0975205061876398, 
                          1.01399087153423, 10.420788530466, 1.5084165224448, 
                          1.46323525034693, 2.30050630727188, 1.0455742822567, 
                          0.396916593664865, 0.276112578159939, 0.0867321859134207, 
                          0.787940961549209, 1.39931415327149, 4.91448118586089, 
                          2.39551550152373, 2.08291844616138, 1.67353143324194, 
                          0.652483430981848, 0.263165822550241, 0.107498717856296, 
                          1.53454251726848, 1.17129688889679, 2.06708280469829, 
                          3.91165644171779, 2.74588910732349, 1.66499320847473, 
                          1.02145416818956, 0.371633336270256, 0.112670158106901, 
                          0.857264438638371, 1.7590640625625, 1.71686658407219, 
                          2.62294018855816, 3.45916114790287, 1.87635185962704, 
                          0.862205884832066, 0.523958801433231, 0.205791955532149, 
                          0.646645383952458, 0.943424739130445, 1.62776721065554, 
                          1.87677409215498, 2.21415705015835, 2.5920177383592, 
                          1.10525460534109, 0.472961105423521, 0.282448363507455, 
                          0.504954014454259, 0.438441714821823, 0.77694120330432, 
                          1.40954408148402, 1.24556204828388, 1.35307720400585, 
                          1.70385674931129, 0.812686154912104, 0.270111273681845, 
                          0.305701280434649, 0.420580126969344, 0.432113761275257, 
                          0.707170907986224, 1.04376196943771, 0.798427737704416, 
                          1.12065725135372, 1.33035714285714, 0.322575366839763, 
                          0.237578345845701, 0.24437789962337, 0.326505855457376, 
                          0.396586297530862, 0.758318763302674, 0.881999483055259, 
                          0.688988121391528, 0.596692087603768, 0.292682926829268)

# visualising the contact matrix
if(visualise) {
  tibble(contacts = contact_matrix_europe) %>%
    mutate(age1 = rep(1:9,9), age2 = rep(1:9,each=9)) %>%
    ggplot() +
    geom_tile(aes(x=age2,y=age1,fill=contacts))
}
# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#
# fixed corrections and delays ####
# ----------------------------------------------------------------------------#
# Fixed corrections ----------------------#
p_report_80plus      = 1
p_underreport_deaths = 1
p_underreport_cases  = p_underreport_cases
p_children_trans     = 1 
# Fixed delays ---------------------------#
G       = 60
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# so-called controls ####
# ----------------------------------------------------------------------------#

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
  p_children_trans=1
}
# ----------------------------------------------------------------------------#