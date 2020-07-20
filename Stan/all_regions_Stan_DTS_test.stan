// Notice I defined real [,]. It is because the output for this function is
  // a two-dimensional array of size S * 6K, so the return expression has to 
  // match the return type.
  real[,] SEIR_dts(
  int S,
  real[] theta,
  real[] x_r,
  int[] x_i) {
    
    // Define free & fix parameters
    int K = x_i[1];         // number of age groups
    real beta;              // transmission rate
    real eta;               // reduction in transmission rate after quarantine
    real xi;                // slope of quarantine implementation
    real nu;                // shift of quarantine implementation
    real tswitch = x_r[1];  // time of control measures
    real tau_1 = x_r[2];    // infection to preclinical
    real tau_2 = x_r[3];    // preclinical to symptoms (tau_1+tau_2 = incubation)
    real q_P = x_r[4];      // contribution of presymptomatics to transmission
    real gt = x_r[5];       // generation time
    real mu;                // infectious duration for symptomatics
    real psi;               // probability of symptoms
    real kappa;             // reduced transmissibility of preclinical and asymptomatics
    real p_tswitch;         // switch function
    real contact[K*K];      // contact matrix, first K values, corresponds to number 
                            // of contact between age class 1 and other classes, etc
    real f_inf[K];          // force of infection
    real init[K*2];         // initial values
    real age_dist[K];       // age distribution of the general population
    real pi;                // number of cases at t0
    
    // Define the compartments storage
    real y[S, (6*K)];
    
    // Estimated parameters
    beta = theta[1];
    eta  = theta[2];
    xi   = theta[3];
    nu   = theta[4];
    pi   = theta[5];
    psi  = theta[6];
    
    // Composite parameters
    mu = (1-q_P)/(gt-1/tau_1-1/tau_2);
    kappa = (q_P*tau_2*psi)/((1-q_P)*mu-(1-psi)*q_P*tau_2);
    
    // Contact matrix
    contact = x_r[6:(5+K*K)];
  
    // Initial conditions of the compartments
    for(k in 1:K){
      age_dist[k] = x_r[5+K*K + k];
      y[1, k] = age_dist[k] * (1-pi);
      y[1, K+k] = age_dist[k] * pi;
      y[1, 2*K+k] = 0;
      y[1, 3*K+k] = 0;
      y[1, 4*K+k] = 0;
      y[1, 5*K+k] = 0;
    }
    
    // Initial p_tswitch (on day 1)
    p_tswitch = switch_eta(1,tswitch,eta,nu,xi);
    
    // Initial force of infection
    for(k in 1:K) {
      f_inf[k] = beta * p_tswitch * sum((to_vector(y[1, (3*K+1):(4*K)]) + 
        kappa*to_vector(y[1, (2*K+1):(3*K)]) + 
        kappa*to_vector(y[1, (4*K+1):(5*K)]))./ to_vector(age_dist) .*
        to_vector(contact[(K*(k-1)+1):(k*K)])); 
    }
    
    // Discrete Time Solver
    for (t in 1:(S-1)){
      // Total number of infectious people (time dependent)
      p_tswitch = switch_eta(t,tswitch,eta,nu,xi);
      
      /*
        Force of infection by age classes (time dependent): 
        beta * p_tswitch * sum((number of infected people by age + 
        + kappa*number of preclinical by age + kappa*number of asympto) / 
        (total number of people by age) * (number of contact by age))
      */
      for (k in 1:K) {
        f_inf[k] = beta * p_tswitch * sum((to_vector(y[t, (3*K+1):(4*K)]) + 
          kappa*to_vector(y[t, (2*K+1):(3*K)]) + 
          kappa*to_vector(y[t, (4*K+1):(5*K)]))./ to_vector(age_dist) .*
          to_vector(contact[(K*(k-1)+1):(k*K)])); 
      }
      
      // Compartments
      for (k in 1:K) {
        // S: susceptible
        y[t+1, k] = - f_inf[k] * (y[t, k]); 
        // E: incubating (not yet infectious)
        y[t+1, K+k] = f_inf[k] * (y[t, k]) - tau_1 * (y[t, K+k]);
        // P: presymptomatic (incubating and infectious)
        y[t+1, 2*K+k] = tau_1 * (y[t, K+k]) - tau_2 * y[t, 2*K+k];
        // I: symptomatic
        y[t+1, 3*K+k] = psi * tau_2 * y[t, 2*K+k] - mu * y[t, 3*K+k];
        // A: asymptomatic
        y[t+1, 4*K+k] = (1-psi) * tau_2 * y[t, 2*K+k] - mu * y[t, 4*K+k];
        // C: cumulative number of infections by date of disease onset
        y[t+1, 5*K+k] = psi * tau_2 * y[t, 2*K+k];
      }
  
    }
    return y[];
  }