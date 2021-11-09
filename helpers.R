calculate_derivatives_leaky <- function(t, x, parameters){
  with(as.list(c(x, parameters)),{
    # alpha = probability of infection given contact
    # phi = percent vaccinated
    
    # per capita contact rates
    if (q > 0){
      rates_vec <- get_contact_rates_with_homophily(phi, q)
      rate_v_and_v <- rates_vec[1]
      rate_u_and_v <- rates_vec[2]
      rate_u_and_u <- rates_vec[3]
    } else {
      rate_v_and_v <- 1
      rate_u_and_v <- 1
      rate_u_and_u <- 1
    }
    
    # if no one is susceptible or everyone/no one is vaccinated, lambda = 0
    if (S_v > 0 && phi != 0){
      lambda_v <- alpha*(rate_u_and_v*I_u + rate_u_and_v*I_x*(1-X_I) + rate_v_and_v*I_h*(1-H_I) + rate_v_and_v*I_v*(1-VE_I))*(1-VE_S) + ext_forcing*(1-VE_S)/N
    } else {
      lambda_v <- 0
    }
    
    if (S_h > 0 && phi != 0){
      lambda_h <- alpha*(rate_u_and_v*I_u + rate_u_and_v*I_x*(1-X_I) + rate_v_and_v*I_h*(1-H_I) + rate_v_and_v*I_v*(1-VE_I))*(1-H_S) + ext_forcing*(1-H_S)/N
    } else {
      lambda_h <- 0
    }
    
    if (S_x > 0 && psi != 0){
      lambda_x <- alpha*(rate_u_and_u*I_u + rate_u_and_u*I_x*(1-X_I) + rate_u_and_v*I_h*(1-H_I) + rate_u_and_v*I_v*(1-VE_I))*(1-X_S) + ext_forcing*(1-X_S)/N
    } else {
      lambda_x <- 0
    }
    
    if (S_u > 0 && phi != 1){
      lambda_u <- alpha*(rate_u_and_u*I_u + rate_u_and_u*I_x*(1-X_I) + rate_u_and_v*I_h*(1-H_I) + rate_u_and_v*I_v*(1-VE_I)) + ext_forcing/N
    } else {
      lambda_u <- 0
    }
    
    dS_v  <- -lambda_v*S_v
    dS_h <- -lambda_h*S_h
    dS_x  <- -lambda_x*S_x
    dS_u  <- -lambda_u*S_u
    
    # make sure S doesn't go negative
    if ((S_v - lambda_v*S_v) < 0){
      dS_v <- -S_v
      lambda_v <- 0
    }
    if ((S_h - lambda_h*S_h) < 0){
      dS_h <- -S_h
      lambda_h <- 0
    }
    if ((S_x - lambda_x*S_x) < 0){
      dS_x <- -S_x
      lambda_x <- 0
    }
    if ((S_u - lambda_u*S_u) < 0){
      dS_u <- -S_u
      lambda_u <- 0
    }
    
    dE_v  <- -dS_v - sigma*E_v
    dE_h  <- -dS_h - sigma*E_h
    dE_x  <- -dS_x - sigma*E_x
    dE_u  <- -dS_u - sigma*E_u
    
    dI_v  <- sigma*E_v - gamma*I_v 
    dI_h  <- sigma*E_h - gamma*I_h 
    dI_x  <- sigma*E_x - (gamma/(1-theta))*I_x
    dI_u  <- sigma*E_u - (gamma/(1-theta))*I_u
    
    dR_v  <- gamma*I_v
    dR_h  <- gamma*I_h
    dR_x  <- (gamma/(1-theta))*I_x 
    dR_u  <- (gamma/(1-theta))*I_u 
    
    return(list(c(dS_v, dS_h, dS_x, dS_u, 
                  dE_v, dE_h, dE_x, dE_u, 
                  dI_v, dI_h, dI_x, dI_u, 
                  dR_v, dR_h, dR_x, dR_u)))
  }
  )
}

run_leaky_model <- function(phi, VE_I, VE_S, theta = 0, q = 0,
                            psi = 0, X_I = 0, X_S = 0, H_I = 0, H_S = 0){
  # phi = fraction vaccinated
  # psi = fraction SARS-CoV-2 experienced
  # theta = impact of testing
  # q = homophily term. 0 is well-mixed, 1 is no mixing
  # v = vaccinated & vaccinated + prior infection
  # x = unvaccinated + prior infection
  # u = unvaccinated
  
  if (ext_forcing == 0){
    I <- 0.0005*N
    E <- 0.0005*N
  } else {
    I <- 0
    E <- 0
  }
  
  I_v_0 <- I*phi*(1-psi)
  E_v_0 <- E*phi*(1-psi)
  
  I_h_0 <- I*phi*psi
  E_h_0 <- E*phi*psi
  
  I_x_0 <- I*(1-phi)*psi
  E_x_0 <- E*(1-phi)*psi
  
  I_u_0 <- I*(1-phi)*(1-psi)
  E_u_0 <- E*(1-phi)*(1-psi)
  
  S_v_0 <- phi*(1-psi)*N - I_v_0 - E_v_0  
  S_h_0 <- phi*psi*N - I_h_0 - E_h_0  
  S_x_0 <- (1-phi)*psi*N - I_x_0 - E_x_0
  S_u_0 <- (1-phi)*(1-psi)*N - I_u_0 - E_u_0
  
  inits <- c(S_v=S_v_0, S_h = S_h_0, S_x = S_x_0, S_u = S_u_0, 
             E_v=E_v_0, E_h = E_h_0, E_x = E_x_0, E_u = E_u_0, 
             I_v=I_v_0, I_h = I_h_0, I_x = I_x_0, I_u = I_u_0, 
             R_v=0,      R_h=0,      R_x=0,        R_u = 0)
  
  parameters <- c(gamma=gamma, alpha=alpha, sigma=sigma, N=N, phi=phi, psi=psi, X_I=X_I, X_S=X_S,
                  VE_I=VE_I, VE_S=VE_S, H_I=H_I, H_S=H_S, theta=theta, q=q)
  
  sim_df <- as.data.frame(lsoda(inits, t, calculate_derivatives_leaky, parameters))
  
  return(sim_df)
}

get_contact_rates_with_homophily <- function(phi, q){
  # INPUTS:
  # phi = fraction vaccinated
  # q = homophily parameter (q = 0 is well-mixed)
  #
  # OUTPUTS:
  # Elements of the contact rate matrix with homophily (units of per capita per day) -- weighted average between well-mixed and homophily scenario, depending on q
  # i.e. j_to_i is the element (i,j): the average rate at which a j individual contacts an i individual per day
  #
  # NOTE: this matrix is symmetric (but the contact matrix is not!)
  # NOTE: here, X and U are indistinguishable 
  # See supp material for derivation
  
  homophily_rate_btwn_v_and_v <- (phi - 1)^2 + 1
  rate_v_and_v <- (1-q) + q*homophily_rate_btwn_v_and_v  
  
  if (phi < 1){
    homophily_rate_btwn_u_and_v <- (1-phi*homophily_rate_btwn_v_and_v)/(1-phi)
    homophily_rate_btwn_u_and_u <- (1 - 2*phi + phi^2*homophily_rate_btwn_v_and_v)/((1-phi)^2)
    
    rate_u_and_v <- (1-q) + q*homophily_rate_btwn_u_and_v
    rate_u_and_u <- (1-q) + q*homophily_rate_btwn_u_and_u
  } else {
    # Hard code the edge case when phi=1 because 0's in the denominator
    # when phi=1 everyone is vaccinated so v_to_v is the only relevant contact rate
    rate_u_and_v <- 0
    rate_u_and_u <- 0
  }
  
  return(c(rate_v_and_v, rate_u_and_v, rate_u_and_u))
}

compute_who_caused_daily_infections <- function(phi, VE_I, VE_S, 
                                                theta = 0, q = 0, 
                                                psi = 0, X_I = 0, X_S = 0,
                                                H_I = 0, H_S = 0){
  # OUTPUT: Number of infections that were caused each day by U, V, and external
  # NOTE: Here we consider X (unvaccinated + prior infection) as part of U
  
  df <- run_leaky_model(phi, VE_I, VE_S, theta, q, psi, X_I, X_S, H_I, H_S)
  I_u <- df$I_u
  I_x <- df$I_x
  I_h <- df$I_h
  I_v <- df$I_v
  S_u <- df$S_u
  S_x <- df$S_x
  S_h <- df$S_h
  S_v <- df$S_v
  
  if (q > 0){
    rates_vec <- get_contact_rates_with_homophily(phi, q)
    rate_v_and_v <- rates_vec[1]
    rate_u_and_v <- rates_vec[2]
    rate_u_and_u <- rates_vec[3]
  } else {
    rate_v_and_v <- 1
    rate_u_and_v <- 1
    rate_u_and_u <- 1
  }
  
  cases_in_v_by_v <- alpha*rate_v_and_v*((I_v*(1-VE_I) + I_h*(1-H_I))*((1-VE_S)*S_v + (1-H_S)*S_h))*dt
  cases_in_v_by_external <- ext_forcing*((1-VE_S)*S_v + (1-H_S)*S_h)/N*dt
  
  # in u by u includes u to u, u to x, x to x, x to u
  cases_in_u_by_u <- alpha*rate_u_and_u*dt*(I_u*S_u + 
                                              I_x*(1 - X_S)*(1 - X_I)*S_x + 
                                              I_u*(1 - X_S)*S_x + 
                                              I_x*(1 - X_I)*S_u)
  cases_in_u_by_external <- ext_forcing*(S_u + (1-X_S)*S_x)/N*dt
  
  # in v by u includes u to v and x to v
  cases_in_v_by_u <- alpha*rate_u_and_v*((I_u + I_x*(1-X_I))*((1-VE_S)*S_v + (1-H_S)*S_h))*dt
  cases_in_u_by_v <- alpha*rate_u_and_v*((I_v*(1-VE_I) + I_h*(1-H_I))*(S_u + (1-X_S)*S_x))*dt
  
  list(cases_in_v_by_v, cases_in_v_by_u, cases_in_u_by_v, cases_in_u_by_u, cases_in_v_by_external, cases_in_u_by_external)
}


compute_who_caused_cases_tot <- function(phi, VE_I, VE_S, theta = 0,
                                         q = 0, psi = 0, X_I = 0, X_S = 0, H_I = 0, H_S = 0){
  # OUTPUT: Percent of who caused cases (U, V, external) over the entire simulation
  
  mylist <- compute_who_caused_daily_infections(phi, VE_I, VE_S, 
                                                theta, q, 
                                                psi, X_I, X_S, H_I, H_S)
  
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

compute_Reff <- function(phi, VE_I, VE_S, theta=0, q=0, psi=0, X_I=0, X_S=0, H_I=0, H_S=0){
  # Numerically compute R_effective as the dominant eigenvalue of the next generation matrix
  # OUTPUT: R_effective 
  
  if (q > 0){
    rates_vec <- get_contact_rates_with_homophily(phi, q)
    rate_v_and_v <- rates_vec[1]
    rate_u_and_v <- rates_vec[2]
    rate_u_and_u <- rates_vec[3]
  } else {
    rate_v_and_v <- 1
    rate_u_and_v <- 1
    rate_u_and_u <- 1
  }
  
  C <- matrix(c(phi*(1-psi)*N*rate_v_and_v, phi*psi*N*rate_v_and_v, (1-phi)*psi*N*rate_u_and_v, (1-phi)*(1-psi)*N*rate_u_and_v,
                phi*(1-psi)*N*rate_v_and_v, phi*psi*N*rate_v_and_v, (1-phi)*psi*N*rate_u_and_v, (1-phi)*(1-psi)*N*rate_u_and_v,
                phi*(1-psi)*N*rate_u_and_v, phi*psi*N*rate_u_and_v, (1-phi)*psi*N*rate_u_and_u, (1-phi)*(1-psi)*N*rate_u_and_u,
                phi*(1-psi)*N*rate_u_and_v, phi*psi*N*rate_u_and_v, (1-phi)*psi*N*rate_u_and_u, (1-phi)*(1-psi)*N*rate_u_and_u),
                nrow = 4, ncol = 4,
                byrow = TRUE)
  
  D_susceptibility <- diag(c(alpha*(1-VE_S), alpha*(1-H_S), alpha*(1-X_S), alpha))
  D_infectiousness <- diag(c((1-VE_I)/gamma, (1-H_I)/gamma, (1-X_I)*(1-theta)/gamma, (1-theta)/gamma))
  
  NGM <- D_susceptibility %*% C %*% D_infectiousness
  eigs <- eigen(NGM)$values
  
  Reff <- max(abs(eigs))
}

compute_tot_infections <- function(phi, VE_I, VE_S, theta = 0, q = 0, psi=0, X_I=0, X_S=0, H_I=0, H_S=0){
  # OUTPUT: total number of infections at t_final (recovered - IC) including infections caused by external forcing
  df <- run_leaky_model(phi, VE_I, VE_S, theta, q, psi, X_I, X_S, H_I, H_S)
  lastrow <- df[length(df$R_v),]
  
  tot_infections <- lastrow$R_v + lastrow$R_h + lastrow$R_x + lastrow$R_u  - 
                    df$E_v[1] - df$E_h[1] - df$E_x[1] - df$E_u[1] -
                    df$I_v[1] - df$I_h[1] - df$I_x[1] - df$I_v[1] 
}

compute_percent_breakthrough_infections <- function(phi, VE_I, VE_S, theta = 0, q = 0, 
                                                    psi=0, X_I=0, X_S=0, H_I=0, H_S=0){
  # OUTPUT: percent of total infections in the vaccinated population
  df <- run_leaky_model(phi, VE_I, VE_S, theta, q, psi, X_I, X_S, H_I, H_S)
  lastrow <- df[length(df$R_v),]
  
  tot_infections <- lastrow$R_v + lastrow$R_h + lastrow$R_x + lastrow$R_u  - 
    df$E_v[1] - df$E_h[1] - df$E_x[1] - df$E_u[1] -
    df$I_v[1] - df$I_h[1] - df$I_x[1] - df$I_v[1]
  
  tot_v_infections <- lastrow$R_v + lastrow$R_h - df$E_v[1] - df$E_h[1] - df$I_v[1] - df$I_h[1]
  percent_breakthrough_infections <- tot_v_infections/tot_infections*100
}

compute_u_infections <- function(phi, VE_I, VE_S, theta = 0, q = 0,
                                 psi=0, X_I=0, X_S=0, H_I=0, H_S=0){
  # OUTPUT: total number of infections in the unvaccinated population
  df <- run_leaky_model(phi, VE_I, VE_S, theta, q, psi, X_I, X_S, H_I, H_S)
  lastrow <- df[length(df$R_v),]
  tot_u_infections <- lastrow$R_x + lastrow$R_u - df$E_x[1] - df$E_u[1] - df$I_x[1] - df$I_u[1]
}

compute_v_infections <- function(phi, VE_I, VE_S, theta = 0, q = 0,
                                 psi=0, X_I=0, X_S=0, H_I=0, H_S=0){
  # OUTPUT: total number of infections in the vaccinated population
  df <- run_leaky_model(phi, VE_I, VE_S, theta, q, psi, X_I, X_S, H_I, H_S)
  lastrow <- df[length(df$R_v),]
  tot_v_infections <- lastrow$R_v + lastrow$R_h - df$E_v[1] - df$E_h[1] - df$I_v[1] - df$I_h[1]
}

compute_num_tests <- function(phi, VE_I, VE_S, theta = 0, freq, inf_period, compliance,
                              q = 0, psi=0, X_I=0, X_S=0, H_I=0, H_S=0){
  # INPUTS:
  # total number of tests administered
  # freq = frequency of testing (in days)
  # inf_period = days spend infectious 
  # compliance = [0,1] testing compliance rate 
  #
  # OUTPUT: total number of tests administered over the simulation
  
  df <- run_leaky_model(phi, VE_I, VE_S, theta, q, psi, X_I, X_S, H_I, H_S)
  df <- df[df$time %% 1 == 0, ] # only consider daily time steps
  prob_test <- 1/freq
  prob_not_caught <- 1 #- min((1/freq)*inf_period*compliance, 1)
  
  df$tests_u <- (df$S_u + df$E_u + df$I_u + df$S_x + df$E_x + df$I_x)*prob_test*compliance +
      (df$R_u + df$R_x)*prob_test*compliance*prob_not_caught
  num_tests <- sum(df$tests_u)
}

compute_dominant_transmission <- function(phi, VE_I, VE_S, theta = 0, q = 0,
                                          psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                          H_I = this_H_I, H_S = this_H_S){
  # OUTPUT:
  # which group (V or U or ext) is contributing most to transmission at a given phi and psi 
  
  who_caused <- unlist(compute_who_caused_cases_tot(phi, VE_I, VE_S, theta, q, psi, X_I,
                                                    X_S, H_I, H_S))
  
  by_v <- who_caused[1] + who_caused[3]
  by_u <- who_caused[2] + who_caused[4]
  by_ext <- who_caused[5] + who_caused[6]
  
  out <- which.max(c(by_v, by_u, by_ext))
  
  if (out == 1){
    out <- ("v")
  } else if (out == 2){
    out <- ("u")
  } else {
    out <- ("ext")
  }
}

compute_infections_averted_per100tests <- function(phi, VE_I, VE_S, theta = 0, q = 0,
                                                   psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                                   H_I = this_H_I, H_S = this_H_S, freq, compliance){
  tot_notesting <- compute_tot_infections(phi, VE_I, VE_S, theta = 0, q, psi, X_I, X_S, H_I, H_S)
  tot_testing <- compute_tot_infections(phi, VE_I, VE_S, theta = theta, q, psi, X_I, X_S, H_I, H_S)
  
  tot_averted <- tot_notesting - tot_testing
  
  num_tests <- compute_num_tests(phi, VE_I, VE_S, theta, freq, 1/gamma, compliance,
                                 q , psi, X_I, X_S, H_I, H_S)
  
  tot_averted_per100 <- tot_averted/num_tests*100
}
