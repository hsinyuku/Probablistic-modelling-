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

# just kept both contact matrices so we don't confuse the two hardwired 
# contact matrices
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
contact_matrix_europe = {c(5.13567073170732, 1.17274819632136, 0.982359525171638,
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
                           0.688988121391528, 0.596692087603768, 0.292682926829268)}
if (region == "Hubei") {
  contact_matrix = contact_matrix_china
} else {
  contact_matrix = contact_matrix_europe
}

# visualising the contact matrix
if(visualise) {
  tibble(contacts = contact_matrix) %>%
    mutate(age1 = rep(1:9,9), age2 = rep(1:9,each=9)) %>%
    ggplot() +
    geom_tile(aes(x=age2,y=age1,fill=contacts))
}
remove(contact_matrix_china, contact_matrix_europe)
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
  contact           = contact_matrix_china,
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