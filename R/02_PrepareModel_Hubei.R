# ----------------------------------------------------------------------------#
# RunModel_Hubei.R
# 
# This file does computations on some preliminary variables (mainly fixed
# parameters) and prepares them for Stan.
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#
# this line is an attempt at making the code execution stop when there are 
# errors in on of the files to be sourced
source("R/01_DataManagement_Hubei.R")
# set this to TRUE if you want visual inspection of paremeters
visualise = F
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
# reduced transmissibility of pre- and a-symptomatics kappa:
# He et al: 44% (25–69%) of secondary cases were infected during the index 
# cases’ presymptomatic stage http://nature.com/articles/s41591-020-0869-5
# Liu estimated 46% (21 – 46%) (S. Funk group) 
# https://wellcomeopenresearch.org/articles/5-58
# Ganyani (Wallinga en Hens) 48% (32-67%) 
# https://medrxiv.org/content/10.1101/2020.03.05.20031815v1

gt = 5.2      # generation time in days
q_P = 0.46    # contribution of presymptomatic infections to the transmission
tau_2 = 1/2.3 # days of incubation without transmission
tau_1 = 1/2.7 # days of incubation with reduced transmission
mu = (1-q_P)/(gt-1/tau_1-1/tau_2) 
              # duration for which symptomatic individuals are infectious
psi = rbeta(1000, p_psi_alpha, p_psi_beta)
kappa = (q_P * tau_2 * psi)/((1-q_P) * mu-(1-psi) * q_P * tau_2)
              # reduction in transmissibility; this is a free parameter
# visualising kappa and psi
if(visualise) {
  tibble(kappa, psi) %>%
    pivot_longer(everything()) %>% 
    ggplot() +
    geom_density(aes(x = value, color = name))
}
remove(psi)
p_children_trans = 1 # dont know what this is

# can we somehow replace the contact matrix?
contact_matrix_china = {c(0.810810810810811, 0.0908559523547268, 0.372736406439194,
                         1.27360250772, 0.200569529052988, 0.375083342749019,
                         0.60252680195839, 0.0934189610338407, 0.0225225225225225,
                         0.0904095466183592, 2.4392523364486, 0.140093983348316,
                         0.706545801082683, 0.942937990573664, 0.27920963239528,
                         0.326366336169345, 0.196893495540358, 0.106045179398683,
                         0.289504965045504, 0.109348487445688, 1.76086956521739,
                         0.923069180041088, 0.93772012267962, 0.965186137047983,
                         0.274120168579709, 0.116564256844925, 0.0773400190233669,
                         0.91820215964926, 0.511898453884688, 0.85680985412458,
                         2.70542635658915, 1.41323192857305, 0.993399938008648,
                         0.719603621821669, 0.146103509716984, 0.07633130138862,
                         0.13119227828341, 0.619819944222649, 0.789700390093264,
                         1.28218991206025, 2.17699115044248, 1.1199461877854,
                         0.514253349451317, 0.496466649026704, 0.101504389707241,
                         0.259078294801222, 0.193808465356441, 0.858341528544101,
                         0.951750199084178, 1.18265232149625, 2.31730769230769,
                         0.977037933291252, 0.606164987575222, 0.4393566902894,
                         0.552747314447092, 0.300880970126328, 0.323770338070664,
                         0.915670466885606, 0.721247101248993, 1.29765260904839,
                         2.76146788990826, 0.959867553314515, 0.340125585278128,
                         0.121809161946423, 0.25799743320884, 0.1956843612527,
                         0.264241585561661, 0.989672909331423, 1.14428055461657,
                         1.36428769674242, 1.96363636363636, 1.0266513139522,
                         0.0447824174066075, 0.211894911958445, 0.197988778289041,
                         0.210517772531686, 0.308554588199316, 1.26474943927563,
                         0.737190168823191, 1.56555579008225, 2.0625)}

# visualising the contact matrix
if(visualise) {
  tibble(contacts = contact_matrix_china) %>%
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
p_underreport_cases  = 1
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
  p_nu      = 1/5 # this value might be wrong, have to check this!
}
# ----------------------------------------------------------------------------#


