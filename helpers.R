calculate_derivatives_leaky <- function(t, x, parameters){
  with(as.list(c(x, parameters)),{
    # alpha = probability of infection given contact
    # phi = percent vaccinated
    
    rates_vec <- get_contact_rates_with_homophily(phi, q)
    vv <- rates_vec[1]
    vu <- rates_vec[2]
    uv <- rates_vec[3]
    uu <- rates_vec[4]
    
    # if no one is susceptible or everyone/no one is vaccinated, lambda = 0
    if (S_v > 0 && phi != 0){
      # lambda_v <- alpha*(uv*I_u + vv*I_v*(1-VE_I))*(1-VE_S) + ext_forcing*(1-VE_S)*phi/S_v
      lambda_v <- alpha*(uv*I_u + vv*I_v*(1-VE_I))*(1-VE_S) + ext_forcing*(1-VE_S)/N
    } else {
      lambda_v <- 0
    }
    
    if (S_u > 0 && phi != 1){
      # lambda_u <- alpha*(uu*I_u + vu*I_v*(1-VE_I)) + ext_forcing*(1-phi)/S_u
      lambda_u <- alpha*(uu*I_u + vu*I_v*(1-VE_I)) + ext_forcing/N
    } else {
      lambda_u <- 0
    }

    dS_v  <- -lambda_v*S_v
    dS_u  <- -lambda_u*S_u
    
    # make sure S can't go negative
    if ((S_v - lambda_v*S_v) < 0){
      dS_v <- -S_v
      lambda_v <- 0
    }
    if ((S_u - lambda_u*S_u) < 0){
      dS_u <- -S_u
      lambda_u <- 0
    }
    
    dE_v  <- -dS_v - sigma*E_v
    dE_u  <- -dS_u - sigma*E_u
    dI_v  <- sigma*E_v - gamma*I_v 
    dI_u  <- sigma*E_u - (gamma/(1-theta))*I_u
    dR_v  <- gamma*I_v
    dR_u  <- (gamma/(1-theta))*I_u 
    
    return(list(c(dS_v, dS_u, dE_v, dE_u, dI_v, dI_u, dR_v, dR_u)))
  }
  )
}

run_leaky_model <- function(phi, VE_I, VE_S, theta = 0, ext_forcing = 1, q = 0){
  # q = homophily term. 0 is well-mixed, 1 is no mixing
  if (ext_forcing == 0){
    I <- 0.0005*N
    E <- 0.0005*N
  } else {
    I <- 0
    E <- 0
  }
  
  I_v_0 <- I*phi
  E_v_0 <- E*phi
  I_u_0 <- I*(1-phi) 
  E_u_0 <- E*(1-phi) 
  
  S_v_0 <- phi*N - I_v_0 - E_v_0  
  S_u_0 <- (1-phi)*N - I_u_0 - E_u_0
  
  inits <- c(S_v=S_v_0, S_u = S_u_0, 
                  E_v=E_v_0, E_u=E_u_0, 
                  I_v=I_v_0, I_u=I_u_0, 
                  R_v=0, R_u=0)
  
  parameters <- c(gamma=gamma, alpha=alpha, sigma=sigma, N=N, phi=phi,
                  VE_I=VE_I, VE_S=VE_S, theta=theta, ext_forcing=ext_forcing, q=q)
  
  sim_df <- as.data.frame(lsoda(inits, t, calculate_derivatives_leaky, parameters))
  
  return(sim_df)
}

get_contact_rates_with_homophily <- function(phi, q){
  
  vv <- 2 - phi
  
  if (phi < 1){
    vu <- (1-phi*vv)/(1-phi)
    uv <- vu*phi/(1-phi)
    uu <- (1-phi*uv)/(1-phi)
  } else {
    vu <- 0
    uv <- 0
    uu <- 1
  }
  
  vv <- (1-q) + q*vv # v to v
  vu <- (1-q) + q*vu # v to u
  uv <- (1-q) + q*uv # u to v
  uu <- (1-q) + q*uu # u to u
  
  return(c(vv, vu, uv, uu))
}

compute_who_caused_daily_infections <- function(phi, this_VE_I, this_VE_S, theta = 0, 
                                                ext_forcing = 1, q = 0){
  # Returns # of infections that were caused each day by u, v and external
  
  df <- run_leaky_model(phi, this_VE_I, this_VE_S, theta, ext_forcing, q)
  I_u <- df$I_u
  I_v <- df$I_v
  S_v <- df$S_v
  S_u <- df$S_u
  
  rates_vec <- get_contact_rates_with_homophily(phi, q)
  vv <- rates_vec[1]
  vu <- rates_vec[2]
  uv <- rates_vec[3]
  uu <- rates_vec[4]

  cases_in_v_by_v <- alpha*vv*I_v*(1-VE_I)*(1-VE_S)*S_v*dt
  cases_in_v_by_external <- phi*(1-VE_S)*ext_forcing*dt # independent of the size of S_v
  
  cases_in_u_by_u <- alpha*uu*I_u*S_u*dt
  cases_in_u_by_external <- (1-phi)*ext_forcing*dt # independent of the size of S_u

  cases_in_v_by_u <- alpha*uv*I_u*(1-VE_S)*S_v*dt
  cases_in_u_by_v <- alpha*vu*I_v*(1-VE_I)*S_u*dt

 
  list(cases_in_v_by_v, cases_in_v_by_u, cases_in_u_by_v, cases_in_u_by_u, cases_in_v_by_external, cases_in_u_by_external)
}


compute_who_caused_cases_tot <- function(this_phi, this_VE_I, this_VE_S, theta = 0,
                                         ext_forcing = 1, q = 0){
  # Returns % of who caused cases over the entire simulation by u, v and external
  
  mylist <- compute_who_caused_daily_infections(this_phi, this_VE_I, this_VE_S, theta,
                                                ext_forcing, q)

  df <- data.frame(time = t)
  
  df$cases_in_v_by_v <- unlist(mylist[[1]])
  df$cases_in_v_by_u <- unlist(mylist[[2]])
  df$cases_in_u_by_v <- unlist(mylist[[3]])
  df$cases_in_u_by_u <- unlist(mylist[[4]])
  df$cases_in_v_by_ext <- unlist(mylist[[5]])
  df$cases_in_u_by_ext <- unlist(mylist[[6]])
  
  total_cases <- sum(df$cases_in_v_by_v + df$cases_in_v_by_u + 
                       df$cases_in_u_by_v + df$cases_in_u_by_u +
                       df$cases_in_v_by_ext + df$cases_in_u_by_ext)
  
  prop_cases_in_v_by_v <- sum(df$cases_in_v_by_v)/total_cases
  prop_cases_in_v_by_u <- sum(df$cases_in_v_by_u)/total_cases
  prop_cases_in_v_by_ext <- sum(df$cases_in_v_by_ext)/total_cases
  
  prop_cases_in_u_by_v <- sum(df$cases_in_u_by_v)/total_cases
  prop_cases_in_u_by_u <- sum(df$cases_in_u_by_u)/total_cases
  prop_cases_in_u_by_ext <- sum(df$cases_in_u_by_ext)/total_cases
  
  list(prop_cases_in_v_by_v, prop_cases_in_v_by_u, prop_cases_in_u_by_v, prop_cases_in_u_by_u, prop_cases_in_v_by_ext, prop_cases_in_u_by_ext)
}

compute_Reff <- function(phi, VE_S, VE_I, theta=0, q=0){
  # OLD - without homophily
  # Reff <- R0*(phi*(1-VE_S)*(1-VE_I) + (1-theta)*(1-phi)) 
  
  rates_vec <- get_contact_rates_with_homophily(phi, q)
  vv <- rates_vec[1]
  vu <- rates_vec[2]
  uv <- rates_vec[3]
  uu <- rates_vec[4]

  C <- matrix(c(phi*N*vv, phi*N*vu, (1-phi)*N*uv, (1-phi)*N*uu), 
              nrow = 2, ncol = 2)

  D_susceptibility <- matrix(c(alpha*(1-VE_S), 0, 0, alpha), 
                             nrow = 2, ncol = 2)
  D_infectiousness <- matrix(c((1-VE_I)/gamma, 0, 0, (1-theta)/gamma), 
                             nrow = 2, ncol = 2)
  
  NGM <- D_susceptibility %*% C %*% D_infectiousness
  eigs <- eigen(NGM)$values
  
  Reff <- max(eigs)
}

compute_tot_infections <- function(this_phi, this_VE_I, this_VE_S, theta = 0, ext_forcing = 1, q = 0){
  # total number of infections at t_final (recovered - IC)
  df <- run_leaky_model(this_phi, this_VE_I, this_VE_S, theta, ext_forcing, q)
  end <- length(df$R_v)
  tot_infections <- df$R_v[end] + df$R_u[end] - df$E_v[1] - df$E_u[1] - df$I_u[1] - df$I_v[1]  
}

compute_percent_v_infections_withext <- function(this_phi, this_VE_I, this_VE_S, theta = 0, ext_forcing = 1, q = 0){
  # percent of infections that are in the vaccinated population
  df <- run_leaky_model(this_phi, this_VE_I, this_VE_S, theta, ext_forcing, q)
  end <- length(df$R_v)
  tot_infections <- df$R_v[end] + df$R_u[end] - df$E_v[1] - df$E_u[1] - df$I_u[1] - df$I_v[1]  
  tot_v_infections <- df$R_v[end] - df$E_v[1] - df$I_v[1]
  percent_breakthrough_infections <- tot_v_infections/tot_infections*100
}

compute_u_infections <- function(this_phi, this_VE_I, this_VE_S, theta = 0, ext_forcing = 1, q = 0){
  # number of infections that are in the unvaccinated population
  df <- run_leaky_model(this_phi, this_VE_I, this_VE_S, theta, ext_forcing, q)
  end <- length(df$R_u)
  tot_u_infections <- df$R_u[end] - df$E_u[1] - df$I_u[1]
}

compute_v_infections <- function(this_phi, this_VE_I, this_VE_S, theta = 0, ext_forcing = 1, q = 0){
  # number of infections that are in the unvaccinated population
  df <- run_leaky_model(this_phi, this_VE_I, this_VE_S, theta, ext_forcing, q)
  end <- length(df$R_v)
  tot_u_infections <- df$R_v[end] - df$E_v[1] - df$I_v[1]
}

compute_num_tests <- function(this_phi, this_VE_I, this_VE_S, theta = 0, freq, inf_period, compliance, ext_forcing = 1,
                              q = 0){
  # total number of tests administered
  # freq = frequency of testing (in days)
  # inf_period = days spend infectious 
  # compliance = [0,1] testing compliance rate 
  df <- run_leaky_model(this_phi, this_VE_I, this_VE_S, theta, ext_forcing, q)
  df <- df[df$time %% 1 == 0, ] # only consider daily time steps
  prob_test <- 1/freq
  prob_not_caught <- 1 - min((1/freq)*inf_period*compliance, 1)
  
  df$tests_u <- (df$S_u + df$E_u + df$I_u)*prob_test*compliance + df$R_u*prob_test*compliance*prob_not_caught
  num_tests <- sum(df$tests_u)
}

# compute_percent_transmission_by_vaxxed <- function(this_phi, this_VE_I, this_VE_S){
#   df <- run_leaky_model(this_phi, this_VE_I, this_VE_S)
#   
#   t_final <- t[length(t)]
#   # only consider ongoing transmission, not ICs 
#   percent_transmission_by_vaxxed <- (df$R_v[t_final] - df$I_v[1] - df$E_v[1])/(df$R_v[t_final] 
#                                                                                + df$R_u[t_final] 
#                                                                                - df$I_v[1] - df$E_v[1]
#                                                                                - df$I_u[1] - df$E_u[1])
#   if (all.equal(df$R_v[t_final], (df$I_v[1] + df$E_v[1])) == TRUE){
#     percent_transmission_by_vaxxed <- 0 # no ongoing transmission by vaccinated
#   }
# 
#   return(percent_transmission_by_vaxxed)
# }

# compute_lambda_v_by_u <- function(this_phi, this_VE_I, this_VE_S){
#   df <- run_leaky_model(this_phi, this_VE_I, this_VE_S)
#   I_u <- df$I_u
#   I_v <- df$I_v
#   S_v <- df$S_v
#   S_u <- df$S_u
#   
#   frac_lambda_v_by_u <- I_u/(I_u + I_v*(1-this_VE_I))
# }

# compute_lambda_tot_by_u <- function(this_phi, this_VE_I, this_VE_S){
#   df <- run_leaky_model(this_phi, this_VE_I, this_VE_S)
#   I_u <- df$I_u
#   I_v <- df$I_v
#   S_v <- df$S_v
#   S_u <- df$S_u
#   
#   frac_lambda_tot_by_u <- (I_u*(1-this_VE_S)*S_v + I_u*S_u)/((I_u + I_v*(1-this_VE_I))*((1-this_VE_S)*S_v + S_u))
# }

# compute_frac_cases_by_u <- function(this_phi, this_VE_I, this_VE_S){
#   df <- run_leaky_model(this_phi, this_VE_I, this_VE_S)
#   I_u <- df$I_u
#   I_v <- df$I_v
#   S_v <- df$S_v
#   S_u <- df$S_u
#   
#   frac_cases_by_u <- (dot(I_u,(1-this_VE_S)*S_v + S_u))/(dot(I_u + I_v*(1-this_VE_I),((1-this_VE_S)*S_v + S_u)))
# }

