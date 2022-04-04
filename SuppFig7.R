# _____________________________________________________________________
# Supplementary Figure 7 - transition points ####
# Show uncertainty in transition points over VE and testing scenarios
# for R0 = 6
# _____________________________________________________________________

# If you want to generate data yourself, start here! Otherwise, skip to line 488 to use pre-generated dataframes.

R0 <- 6
alpha <- R0*gamma/N # transmissibility

phi_vec <- seq(0.5, 1, by = 0.05) # by = 0.01 for fine-grid plots
psi_vec <- seq(0, 1, by = 0.05)

for (i in 1){
  
  baseline_df <- expand.grid(phi = phi_vec, psi = psi_vec)
  baseline_df$breakthroughs_notesting <- NA
  baseline_df$breakthroughs_99 <- NA
  baseline_df$breakthroughs_50 <- NA
  baseline_df$v_transmission_notesting <- NA
  baseline_df$v_transmission_99 <- NA
  baseline_df$v_transmission_50 <- NA
  baseline_df$hosp_notesting <- NA
  baseline_df$hosp_99 <- NA
  baseline_df$hosp_50 <- NA
  baseline_df$VE <- "baseline"
  
  boosted_df <- expand.grid(phi = phi_vec, psi = psi_vec)
  boosted_df$breakthroughs_notesting <- NA
  boosted_df$breakthroughs_99 <- NA
  boosted_df$breakthroughs_50 <- NA
  boosted_df$v_transmission_notesting <- NA
  boosted_df$v_transmission_99 <- NA
  boosted_df$v_transmission_50 <- NA
  boosted_df$hosp_notesting <- NA
  boosted_df$hosp_99 <- NA
  boosted_df$hosp_50 <- NA
  boosted_df$VE <- "boosted"
  
  waning_df <- expand.grid(phi = phi_vec, psi = psi_vec)
  waning_df$breakthroughs_notesting <- NA
  waning_df$breakthroughs_99 <- NA
  waning_df$breakthroughs_50 <- NA
  waning_df$v_transmission_notesting <- NA
  waning_df$v_transmission_99 <- NA
  waning_df$v_transmission_50 <- NA
  waning_df$hosp_notesting <- NA
  waning_df$hosp_99 <- NA
  waning_df$hosp_50 <- NA
  waning_df$VE <- "low"
  
  omicron_df <- expand.grid(phi = phi_vec, psi = psi_vec)
  omicron_df$breakthroughs_notesting <- NA
  omicron_df$breakthroughs_99 <- NA
  omicron_df$breakthroughs_50 <- NA
  omicron_df$breakthroughs_biwk <- NA
  omicron_df$v_transmission_notesting <- NA
  omicron_df$v_transmission_99 <- NA
  omicron_df$v_transmission_50 <- NA
  omicron_df$v_transmission_biwk <- NA
  omicron_df$hosp_notesting <- NA
  omicron_df$hosp_99 <- NA
  omicron_df$hosp_50 <- NA
  omicron_df$hosp_biwk <- NA
  omicron_df$VE <- "omicron"
  
  for (i in 1:dim(omicron_df)[1]){
    
    # Baseline scenario
    baseline_df$breakthroughs_notesting[i] <- compute_percent_breakthrough_infections(baseline_df$phi[i],
                                                                                      VE_I = baseline_VE_I, VE_S = baseline_VE_S,
                                                                                      theta = 0, q = this_q,
                                                                                      baseline_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                                      H_I = baseline_H_I, H_S = baseline_H_S)
    baseline_df$breakthroughs_99[i] <- compute_percent_breakthrough_infections(baseline_df$phi[i],
                                                                               VE_I = baseline_VE_I, VE_S = baseline_VE_S,
                                                                               theta = theta_99, q = this_q,
                                                                               baseline_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                               H_I = baseline_H_I, H_S = baseline_H_S)
    baseline_df$breakthroughs_50[i] <- compute_percent_breakthrough_infections(baseline_df$phi[i],
                                                                               VE_I = baseline_VE_I, VE_S = baseline_VE_S,
                                                                               theta = theta_50, q = this_q,
                                                                               baseline_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                               H_I = baseline_H_I, H_S = baseline_H_S)
    
    baseline_df$v_transmission_notesting[i] <- 100 - compute_dominant_transmission(baseline_df$phi[i],
                                                                                   VE_I = baseline_VE_I, VE_S = baseline_VE_S,
                                                                                   theta = 0, q = this_q,
                                                                                   baseline_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                                   H_I = baseline_H_I, H_S = baseline_H_S) * 100
    baseline_df$v_transmission_99[i] <- 100 - compute_dominant_transmission(baseline_df$phi[i],
                                                                            VE_I = baseline_VE_I, VE_S = baseline_VE_S,
                                                                            theta = theta_99, q = this_q,
                                                                            baseline_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                            H_I = baseline_H_I, H_S = baseline_H_S) * 100
    baseline_df$v_transmission_50[i] <- 100 - compute_dominant_transmission(baseline_df$phi[i],
                                                                            VE_I = baseline_VE_I, VE_S = baseline_VE_S,
                                                                            theta = theta_50, q = this_q,
                                                                            baseline_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                            H_I = baseline_H_I, H_S = baseline_H_S) * 100
    
    baseline_df$hosp_notesting[i] <- compute_percent_breakthrough_hosp(baseline_df$phi[i], VE_I = baseline_VE_I, VE_S = baseline_VE_S, VE_P = baseline_VE_P, 
                                                                       infection_hosp_rate = infection_hosp_rate_delta,
                                                                       theta = 0, q = this_q, 
                                                                       baseline_df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                                       H_I = baseline_H_I, H_S = baseline_H_S, H_P = baseline_H_P)
    
    baseline_df$hosp_99[i] <- compute_percent_breakthrough_hosp(baseline_df$phi[i], VE_I = baseline_VE_I, VE_S = baseline_VE_S, VE_P = baseline_VE_P, 
                                                                infection_hosp_rate = infection_hosp_rate_delta,
                                                                theta = theta_99, q = this_q, 
                                                                baseline_df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                                H_I = baseline_H_I, H_S = baseline_H_S, H_P = baseline_H_P)
    baseline_df$hosp_50[i] <- compute_percent_breakthrough_hosp(baseline_df$phi[i],  VE_I = baseline_VE_I, VE_S = baseline_VE_S, VE_P = baseline_VE_P, 
                                                                infection_hosp_rate = infection_hosp_rate_delta,
                                                                theta = theta_50, q = this_q, 
                                                                baseline_df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                                H_I = baseline_H_I, H_S = baseline_H_S, H_P = baseline_H_P)
    
    
    # Boosted scenario
    boosted_df$breakthroughs_notesting[i] <- compute_percent_breakthrough_infections(boosted_df$phi[i],
                                                                                     VE_I = boosted_VE_I, VE_S = boosted_VE_S,
                                                                                     theta = 0, q = this_q,
                                                                                     boosted_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                                     H_I = boosted_H_I, H_S = boosted_H_S)
    boosted_df$breakthroughs_99[i] <- compute_percent_breakthrough_infections(boosted_df$phi[i],
                                                                              VE_I = boosted_VE_I, VE_S = boosted_VE_S,
                                                                              theta = theta_99, q = this_q,
                                                                              boosted_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                              H_I = boosted_H_I, H_S = boosted_H_S)
    boosted_df$breakthroughs_50[i] <- compute_percent_breakthrough_infections(boosted_df$phi[i],
                                                                              VE_I = boosted_VE_I, VE_S = boosted_VE_S,
                                                                              theta = theta_50, q = this_q,
                                                                              boosted_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                              H_I = boosted_H_I, H_S = boosted_H_S)
    
    boosted_df$v_transmission_notesting[i] <- 100 - compute_dominant_transmission(boosted_df$phi[i],
                                                                                  VE_I = boosted_VE_I, VE_S = boosted_VE_S,
                                                                                  theta = 0, q = this_q,
                                                                                  boosted_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                                  H_I = boosted_H_I, H_S = boosted_H_S) * 100
    boosted_df$v_transmission_99[i] <- 100 - compute_dominant_transmission(boosted_df$phi[i],
                                                                           VE_I = boosted_VE_I, VE_S = boosted_VE_S,
                                                                           theta = theta_99, q = this_q,
                                                                           boosted_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                           H_I = boosted_H_I, H_S = boosted_H_S) * 100
    boosted_df$v_transmission_50[i] <- 100 - compute_dominant_transmission(boosted_df$phi[i],
                                                                           VE_I = boosted_VE_I, VE_S = boosted_VE_S,
                                                                           theta = theta_50, q = this_q,
                                                                           boosted_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                           H_I = boosted_H_I, H_S = boosted_H_S) * 100
    
    boosted_df$hosp_notesting[i] <- compute_percent_breakthrough_hosp(boosted_df$phi[i], VE_I = boosted_VE_I, VE_S = boosted_VE_S, VE_P = boosted_VE_P, 
                                                                      infection_hosp_rate = infection_hosp_rate_delta,
                                                                      theta = 0, q = this_q, 
                                                                      boosted_df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                                      H_I = boosted_H_I, H_S = boosted_H_S, H_P = boosted_H_P)
    boosted_df$hosp_99[i] <- compute_percent_breakthrough_hosp(boosted_df$phi[i], VE_I = boosted_VE_I, VE_S = boosted_VE_S, VE_P = boosted_VE_P, 
                                                               infection_hosp_rate = infection_hosp_rate_delta,
                                                               theta = theta_99, q = this_q, 
                                                               boosted_df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                               H_I = boosted_H_I, H_S = boosted_H_S, H_P = boosted_H_P)
    boosted_df$hosp_50[i] <- compute_percent_breakthrough_hosp(boosted_df$phi[i], VE_I = boosted_VE_I, VE_S = boosted_VE_S, VE_P = boosted_VE_P, 
                                                               infection_hosp_rate = infection_hosp_rate_delta,
                                                               theta = theta_50, q = this_q, 
                                                               boosted_df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                               H_I = boosted_H_I, H_S = boosted_H_S, H_P = boosted_H_P)
    
    # Low VE scenario
    waning_df$breakthroughs_notesting[i] <- compute_percent_breakthrough_infections(waning_df$phi[i],
                                                                                    VE_I = waning_VE_I, VE_S = waning_VE_S,
                                                                                    theta = 0, q = this_q,
                                                                                    waning_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                                    H_I = waning_H_I, H_S = waning_H_S)
    waning_df$breakthroughs_99[i] <- compute_percent_breakthrough_infections(waning_df$phi[i],
                                                                             VE_I = waning_VE_I, VE_S = waning_VE_S,
                                                                             theta = theta_99, q = this_q,
                                                                             waning_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                             H_I = waning_H_I, H_S = waning_H_S)
    waning_df$breakthroughs_50[i] <- compute_percent_breakthrough_infections(waning_df$phi[i],
                                                                             VE_I = waning_VE_I, VE_S = waning_VE_S,                                
                                                                             theta = theta_50, q = this_q,
                                                                             waning_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                             H_I = waning_H_I, H_S = waning_H_S)
    
    waning_df$v_transmission_notesting[i] <- 100 - compute_dominant_transmission(waning_df$phi[i],
                                                                                 VE_I = waning_VE_I, VE_S = waning_VE_S,
                                                                                 theta = 0, q = this_q,
                                                                                 waning_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                                 H_I = waning_H_I, H_S = waning_H_S) * 100
    waning_df$v_transmission_99[i] <- 100 - compute_dominant_transmission(waning_df$phi[i],
                                                                          VE_I = waning_VE_I, VE_S = waning_VE_S,
                                                                          theta = theta_99, q = this_q,
                                                                          waning_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                          H_I = waning_H_I, H_S = waning_H_S) * 100
    waning_df$v_transmission_50[i] <- 100 - compute_dominant_transmission(waning_df$phi[i],
                                                                          VE_I = waning_VE_I, VE_S = waning_VE_S,
                                                                          theta = theta_50, q = this_q,
                                                                          waning_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                          H_I = waning_H_I, H_S = waning_H_S) * 100
    
    waning_df$hosp_notesting[i] <- compute_percent_breakthrough_hosp(waning_df$phi[i], VE_I = waning_VE_I, VE_S = waning_VE_S, VE_P = waning_VE_P, 
                                                                     infection_hosp_rate = infection_hosp_rate_delta,
                                                                     theta = 0, q = this_q, 
                                                                     waning_df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                                     H_I = waning_H_I, H_S = waning_H_S, H_P = waning_H_P)
    waning_df$hosp_99[i] <- compute_percent_breakthrough_hosp(waning_df$phi[i], VE_I = waning_VE_I, VE_S = waning_VE_S, VE_P = waning_VE_P,  
                                                              infection_hosp_rate = infection_hosp_rate_delta,
                                                              theta = theta_99, q = this_q, 
                                                              waning_df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                              H_I = waning_H_I, H_S = waning_H_S, H_P = waning_H_P)
    waning_df$hosp_50[i] <- compute_percent_breakthrough_hosp(waning_df$phi[i], VE_I = waning_VE_I, VE_S = waning_VE_S, VE_P = waning_VE_P,  
                                                              infection_hosp_rate = infection_hosp_rate_delta,
                                                              theta = theta_50, q = this_q, 
                                                              waning_df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                              H_I = waning_H_I, H_S = waning_H_S, H_P = waning_H_P)
    
    # Omicron scenario
    omicron_df$breakthroughs_notesting[i] <- compute_percent_breakthrough_infections(omicron_df$phi[i],
                                                                                     VE_I = omicron_VE_I, VE_S = omicron_VE_S,
                                                                                     theta = 0, q = this_q,
                                                                                     omicron_df$psi[i], X_I = omicron_X_I, X_S = omicron_X_S,
                                                                                     H_I = omicron_H_I, H_S = omicron_H_S)
    omicron_df$breakthroughs_99[i] <- compute_percent_breakthrough_infections(omicron_df$phi[i],
                                                                              VE_I = omicron_VE_I, VE_S = omicron_VE_S,
                                                                              theta = theta_99, q = this_q,
                                                                              omicron_df$psi[i], X_I = omicron_X_I, X_S = omicron_X_S,
                                                                              H_I = omicron_H_I, H_S = omicron_H_S)
    omicron_df$breakthroughs_50[i] <- compute_percent_breakthrough_infections(omicron_df$phi[i],
                                                                              VE_I = omicron_VE_I, VE_S = omicron_VE_S,
                                                                              theta = theta_50, q = this_q,
                                                                              omicron_df$psi[i], X_I = omicron_X_I, X_S = omicron_X_S,
                                                                              H_I = omicron_H_I, H_S = omicron_H_S)
    omicron_df$breakthroughs_biwk[i] <- compute_percent_breakthrough_infections(omicron_df$phi[i],
                                                                                VE_I = omicron_VE_I, VE_S = omicron_VE_S,
                                                                                theta = theta_99_biwk, q = this_q,
                                                                                omicron_df$psi[i], X_I = omicron_X_I, X_S = omicron_X_S,
                                                                                H_I = omicron_H_I, H_S = omicron_H_S)
    
    omicron_df$v_transmission_notesting[i] <- 100 - compute_dominant_transmission(omicron_df$phi[i],
                                                                                  VE_I = omicron_VE_I, VE_S = omicron_VE_S,
                                                                                  theta = 0, q = this_q,
                                                                                  omicron_df$psi[i], X_I = omicron_X_I, X_S = omicron_X_S,
                                                                                  H_I = omicron_H_I, H_S = omicron_H_S) * 100
    omicron_df$v_transmission_99[i] <- 100 - compute_dominant_transmission(omicron_df$phi[i],
                                                                           VE_I = omicron_VE_I, VE_S = omicron_VE_S,
                                                                           theta = theta_99, q = this_q,
                                                                           omicron_df$psi[i], X_I = omicron_X_I, X_S = omicron_X_S,
                                                                           H_I = omicron_H_I, H_S = omicron_H_S) * 100
    omicron_df$v_transmission_50[i] <- 100 - compute_dominant_transmission(omicron_df$phi[i],
                                                                           VE_I = omicron_VE_I, VE_S = omicron_VE_S,
                                                                           theta = theta_50, q = this_q,
                                                                           omicron_df$psi[i], X_I = omicron_X_I, X_S = omicron_X_S,
                                                                           H_I = omicron_H_I, H_S = omicron_H_S) * 100
    omicron_df$v_transmission_biwk[i] <- 100 - compute_dominant_transmission(omicron_df$phi[i],
                                                                             VE_I = omicron_VE_I, VE_S = omicron_VE_S,
                                                                             theta = theta_99_biwk, q = this_q,
                                                                             omicron_df$psi[i], X_I = omicron_X_I, X_S = omicron_X_S,
                                                                             H_I = omicron_H_I, H_S = omicron_H_S) * 100
    
    omicron_df$hosp_notesting[i] <- compute_percent_breakthrough_hosp(omicron_df$phi[i], VE_I = omicron_VE_I, VE_S = omicron_VE_S, VE_P = omicron_VE_P, 
                                                                      infection_hosp_rate = infection_hosp_rate_delta,
                                                                      theta = 0, q = this_q, 
                                                                      omicron_df$psi[i], X_I = omicron_X_I, X_S = omicron_X_S, X_P = omicron_X_P,
                                                                      H_I = omicron_H_I, H_S = omicron_H_S, H_P = omicron_H_P)
    omicron_df$hosp_99[i] <- compute_percent_breakthrough_hosp(omicron_df$phi[i], VE_I = omicron_VE_I, VE_S = omicron_VE_S, VE_P = omicron_VE_P, 
                                                               infection_hosp_rate = infection_hosp_rate_delta,
                                                               theta = theta_99, q = this_q, 
                                                               omicron_df$psi[i], X_I = omicron_X_I, X_S = omicron_X_S, X_P = omicron_X_P,
                                                               H_I = omicron_H_I, H_S = omicron_H_S, H_P = omicron_H_P)
    omicron_df$hosp_50[i] <- compute_percent_breakthrough_hosp(omicron_df$phi[i], VE_I = omicron_VE_I, VE_S = omicron_VE_S, VE_P = omicron_VE_P, 
                                                               infection_hosp_rate = infection_hosp_rate_delta,
                                                               theta = theta_50, q = this_q, 
                                                               omicron_df$psi[i], X_I = omicron_X_I, X_S = omicron_X_S, X_P = omicron_X_P,
                                                               H_I = omicron_H_I, H_S = omicron_H_S, H_P = omicron_H_P)
    omicron_df$hosp_biwk[i] <- compute_percent_breakthrough_hosp(omicron_df$phi[i], VE_I = omicron_VE_I, VE_S = omicron_VE_S, VE_P = omicron_VE_P,  
                                                                 infection_hosp_rate = infection_hosp_rate_delta,
                                                                 theta = theta_50, q = this_q, 
                                                                 omicron_df$psi[i], X_I = omicron_X_I, X_S = omicron_X_S, X_P = omicron_X_P,
                                                                 H_I = omicron_H_I, H_S = omicron_H_S, H_P = omicron_H_P)
    
  }
  
  # save dateframes
  saveRDS(waning_df,"df_Fig7_waning_R06.RData")
  saveRDS(baseline_df,"df_Fig7_baseline_R06.RData")
  saveRDS(boosted_df,"df_Fig7_boosted_R06.RData")
  saveRDS(omicron_df,"df_Fig7_omicron_R06.RData")
  
  # compute summary statistics
  VEs <- c(rep(baseline_VE_S,3),rep(boosted_VE_S,3),rep(waning_VE_S,3),rep(omicron_VE_S,4))
  df <- data.frame(VE = VEs)
  df$testing <- c(rep(c(0,50,99),3),c(0,50,99,299))
  df$max_inf_transition <- NA
  df$min_inf_transition <- NA
  df$max_trnsmsn_transition <- NA
  df$min_trnsmsn_transition <- NA
  df$max_hosp_transition <- NA
  df$min_hosp_transition <- NA
  
  # Baseline VE
  baseline_inf_transitions_notesting <- baseline_df[baseline_df$breakthroughs_notesting >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==0 & df$VE==baseline_VE_S,]$min_inf_transition <- min(baseline_inf_transitions_notesting$phi)
  df[df$testing==0 & df$VE==baseline_VE_S,]$max_inf_transition <-  max(baseline_inf_transitions_notesting$phi)
  baseline_inf_transitions_99 <- baseline_df[baseline_df$breakthroughs_99 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==99 & df$VE==baseline_VE_S,]$min_inf_transition <- min(baseline_inf_transitions_99$phi)
  df[df$testing==99 & df$VE==baseline_VE_S,]$max_inf_transition <- max(baseline_inf_transitions_99$phi)
  baseline_inf_transitions_50 <- baseline_df[baseline_df$breakthroughs_50 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==50 & df$VE==baseline_VE_S,]$min_inf_transition <- min(baseline_inf_transitions_50$phi)
  df[df$testing==50 & df$VE==baseline_VE_S,]$max_inf_transition <- max(baseline_inf_transitions_50$phi)
  
  baseline_trnsmsn_transitions_notesting <- baseline_df[baseline_df$v_transmission_notesting >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==0 & df$VE==baseline_VE_S,]$min_trnsmsn_transition <- min(baseline_trnsmsn_transitions_notesting$phi)
  df[df$testing==0 & df$VE==baseline_VE_S,]$max_trnsmsn_transition <-  max(baseline_trnsmsn_transitions_notesting$phi)
  baseline_trnsmsn_transitions_99 <- baseline_df[baseline_df$v_transmission_99 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==99 & df$VE==baseline_VE_S,]$min_trnsmsn_transition <- min(baseline_trnsmsn_transitions_99$phi)
  df[df$testing==99 & df$VE==baseline_VE_S,]$max_trnsmsn_transition <- max(baseline_trnsmsn_transitions_99$phi)
  baseline_trnsmsn_transitions_50 <- baseline_df[baseline_df$v_transmission_50 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==50 & df$VE==baseline_VE_S,]$min_trnsmsn_transition <- min(baseline_trnsmsn_transitions_50$phi)
  df[df$testing==50 & df$VE==baseline_VE_S,]$max_trnsmsn_transition <- max(baseline_trnsmsn_transitions_50$phi)
  
  baseline_hosp_transitions_notesting <- baseline_df[baseline_df$hosp_notesting >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==0 & df$VE==baseline_VE_S,]$min_hosp_transition <- min(baseline_hosp_transitions_notesting$phi)
  df[df$testing==0 & df$VE==baseline_VE_S,]$max_hosp_transition <-  max(baseline_hosp_transitions_notesting$phi)
  baseline_hosp_transitions_99 <- baseline_df[baseline_df$hosp_99 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==99 & df$VE==baseline_VE_S,]$min_hosp_transition <- min(baseline_hosp_transitions_99$phi)
  df[df$testing==99 & df$VE==baseline_VE_S,]$max_hosp_transition <- max(baseline_hosp_transitions_99$phi)
  baseline_hosp_transitions_50 <- baseline_df[baseline_df$hosp_50 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==50 & df$VE==baseline_VE_S,]$min_hosp_transition <- min(baseline_hosp_transitions_50$phi)
  df[df$testing==50 & df$VE==baseline_VE_S,]$max_hosp_transition <- max(baseline_hosp_transitions_50$phi)
  
  # Boosted VE
  boosted_inf_transitions_notesting <- boosted_df[boosted_df$breakthroughs_notesting >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==0 & df$VE==boosted_VE_S,]$min_inf_transition <- min(boosted_inf_transitions_notesting$phi)
  df[df$testing==0 & df$VE==boosted_VE_S,]$max_inf_transition <-  max(boosted_inf_transitions_notesting$phi)
  boosted_inf_transitions_99 <- baseline_df[boosted_df$breakthroughs_99 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==99 & df$VE==boosted_VE_S,]$min_inf_transition <- min(boosted_inf_transitions_99$phi)
  df[df$testing==99 & df$VE==boosted_VE_S,]$max_inf_transition <- max(boosted_inf_transitions_99$phi)
  boosted_inf_transitions_50 <- boosted_df[boosted_df$breakthroughs_50 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==50 & df$VE==boosted_VE_S,]$min_inf_transition <- min(boosted_inf_transitions_50$phi)
  df[df$testing==50 & df$VE==boosted_VE_S,]$max_inf_transition <- max(boosted_inf_transitions_50$phi)
  
  boosted_trnsmsn_transitions_notesting <- boosted_df[boosted_df$v_transmission_notesting >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==0 & df$VE==boosted_VE_S,]$min_trnsmsn_transition <- min(boosted_trnsmsn_transitions_notesting$phi)
  df[df$testing==0 & df$VE==boosted_VE_S,]$max_trnsmsn_transition <- max(boosted_trnsmsn_transitions_notesting$phi)
  boosted_trnsmsn_transitions_99 <- boosted_df[boosted_df$v_transmission_99 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==99 & df$VE==boosted_VE_S,]$min_trnsmsn_transition <- min(boosted_trnsmsn_transitions_99$phi)
  df[df$testing==99 & df$VE==boosted_VE_S,]$max_trnsmsn_transition <- max(boosted_trnsmsn_transitions_99$phi)
  boosted_trnsmsn_transitions_50 <- boosted_df[boosted_df$v_transmission_50 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==50 & df$VE==boosted_VE_S,]$min_trnsmsn_transition <- min(boosted_trnsmsn_transitions_50$phi)
  df[df$testing==50 & df$VE==boosted_VE_S,]$max_trnsmsn_transition <- max(boosted_trnsmsn_transitions_50$phi)
  
  boosted_hosp_transitions_notesting <- boosted_df[boosted_df$hosp_notesting >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==0 & df$VE==boosted_VE_S,]$min_hosp_transition <- min(boosted_hosp_transitions_notesting$phi)
  df[df$testing==0 & df$VE==boosted_VE_S,]$max_hosp_transition <- max(boosted_hosp_transitions_notesting$phi)
  boosted_hosp_transitions_99 <- boosted_df[boosted_df$hosp_99 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==99 & df$VE==boosted_VE_S,]$min_hosp_transition <- min(boosted_hosp_transitions_99$phi)
  df[df$testing==99 & df$VE==boosted_VE_S,]$max_hosp_transition <- max(boosted_hosp_transitions_99$phi)
  boosted_hosp_transitions_50 <- boosted_df[boosted_df$hosp_50 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==50 & df$VE==boosted_VE_S,]$min_hosp_transition <- min(boosted_hosp_transitions_50$phi)
  df[df$testing==50 & df$VE==boosted_VE_S,]$max_hosp_transition <- max(boosted_hosp_transitions_50$phi)
  
  
  # Low VE
  waning_inf_transitions_notesting <- waning_df[waning_df$breakthroughs_notesting >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==0 & df$VE==waning_VE_S,]$min_inf_transition <- min(waning_inf_transitions_notesting$phi)
  df[df$testing==0 & df$VE==waning_VE_S,]$max_inf_transition <-  max(waning_inf_transitions_notesting$phi)
  waning_inf_transitions_99 <- waning_df[waning_df$breakthroughs_99 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==99 & df$VE==waning_VE_S,]$min_inf_transition <- min(waning_inf_transitions_99$phi)
  df[df$testing==99 & df$VE==waning_VE_S,]$max_inf_transition <- max(waning_inf_transitions_99$phi)
  waning_inf_transitions_50 <- waning_df[waning_df$breakthroughs_50 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==50 & df$VE==waning_VE_S,]$min_inf_transition <- min(waning_inf_transitions_50$phi)
  df[df$testing==50 & df$VE==waning_VE_S,]$max_inf_transition <- max(waning_inf_transitions_50$phi)
  
  waning_trnsmsn_transitions_notesting <- waning_df[waning_df$v_transmission_notesting >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==0 & df$VE==waning_VE_S,]$min_trnsmsn_transition <- min(waning_trnsmsn_transitions_notesting$phi)
  df[df$testing==0 & df$VE==waning_VE_S,]$max_trnsmsn_transition <- max(waning_trnsmsn_transitions_notesting$phi)
  waning_trnsmsn_transitions_99 <- waning_df[waning_df$v_transmission_99 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==99 & df$VE==waning_VE_S,]$min_trnsmsn_transition <- min(waning_trnsmsn_transitions_99$phi)
  df[df$testing==99 & df$VE==waning_VE_S,]$max_trnsmsn_transition <- max(waning_trnsmsn_transitions_99$phi)
  waning_trnsmsn_transitions_50 <- waning_df[waning_df$v_transmission_50 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==50 & df$VE==waning_VE_S,]$min_trnsmsn_transition <- min(waning_trnsmsn_transitions_50$phi)
  df[df$testing==50 & df$VE==waning_VE_S,]$max_trnsmsn_transition <- max(waning_trnsmsn_transitions_50$phi)
  
  waning_hosp_transitions_notesting <- waning_df[waning_df$hosp_notesting >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==0 & df$VE==waning_VE_S,]$min_hosp_transition <- min(waning_hosp_transitions_notesting$phi)
  df[df$testing==0 & df$VE==waning_VE_S,]$max_hosp_transition <- max(waning_hosp_transitions_notesting$phi)
  waning_hosp_transitions_99 <- waning_df[waning_df$hosp_99 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==99 & df$VE==waning_VE_S,]$min_hosp_transition <- min(waning_hosp_transitions_99$phi)
  df[df$testing==99 & df$VE==waning_VE_S,]$max_hosp_transition <- max(waning_hosp_transitions_99$phi)
  waning_hosp_transitions_50 <- waning_df[waning_df$hosp_50 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==50 & df$VE==waning_VE_S,]$min_hosp_transition <- min(waning_hosp_transitions_50$phi)
  df[df$testing==50 & df$VE==waning_VE_S,]$max_hosp_transition <- max(waning_hosp_transitions_50$phi)
  
  
  # Omicron VE
  omicron_inf_transitions_notesting <- omicron_df[omicron_df$breakthroughs_notesting >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==0 & df$VE==omicron_VE_S,]$min_inf_transition <- min(omicron_inf_transitions_notesting$phi)
  df[df$testing==0 & df$VE==omicron_VE_S,]$max_inf_transition <-  max(omicron_inf_transitions_notesting$phi)
  omicron_inf_transitions_99 <- omicron_df[omicron_df$breakthroughs_99 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==99 & df$VE==omicron_VE_S,]$min_inf_transition <- min(omicron_inf_transitions_99$phi)
  df[df$testing==99 & df$VE==omicron_VE_S,]$max_inf_transition <- max(omicron_inf_transitions_99$phi)
  omicron_inf_transitions_50 <- omicron_df[omicron_df$breakthroughs_50 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==50 & df$VE==omicron_VE_S,]$min_inf_transition <- min(omicron_inf_transitions_50$phi)
  df[df$testing==50 & df$VE==omicron_VE_S,]$max_inf_transition <- max(omicron_inf_transitions_50$phi)
  omicron_inf_transitions_biwk <- omicron_df[omicron_df$breakthroughs_biwk >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==299 & df$VE==omicron_VE_S,]$min_inf_transition <- min(omicron_inf_transitions_biwk$phi)
  df[df$testing==299 & df$VE==omicron_VE_S,]$max_inf_transition <- max(omicron_inf_transitions_biwk$phi)
  
  omicron_trnsmsn_transitions_notesting <- omicron_df[omicron_df$v_transmission_notesting >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==0 & df$VE==omicron_VE_S,]$min_trnsmsn_transition <- min(omicron_trnsmsn_transitions_notesting$phi)
  df[df$testing==0 & df$VE==omicron_VE_S,]$max_trnsmsn_transition <-  max(omicron_trnsmsn_transitions_notesting$phi)
  omicron_trnsmsn_transitions_99 <- omicron_df[omicron_df$v_transmission_99 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==99 & df$VE==omicron_VE_S,]$min_trnsmsn_transition <- min(omicron_trnsmsn_transitions_99$phi)
  df[df$testing==99 & df$VE==omicron_VE_S,]$max_trnsmsn_transition <- max(omicron_trnsmsn_transitions_99$phi)
  omicron_trnsmsn_transitions_50 <- omicron_df[omicron_df$v_transmission_50 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==50 & df$VE==omicron_VE_S,]$min_trnsmsn_transition <- min(omicron_trnsmsn_transitions_50$phi)
  df[df$testing==50 & df$VE==omicron_VE_S,]$max_trnsmsn_transition <- max(omicron_trnsmsn_transitions_50$phi)
  omicron_trnsmsn_transitions_biwk <- omicron_df[omicron_df$v_transmission_biwk >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==299 & df$VE==omicron_VE_S,]$min_trnsmsn_transition <- min(omicron_trnsmsn_transitions_biwk$phi)
  df[df$testing==299 & df$VE==omicron_VE_S,]$max_trnsmsn_transition <- max(omicron_trnsmsn_transitions_biwk$phi)
  
  omicron_hosp_transitions_notesting <- omicron_df[omicron_df$hosp_notesting >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==0 & df$VE==omicron_VE_S,]$min_hosp_transition <- min(omicron_hosp_transitions_notesting$phi)
  df[df$testing==0 & df$VE==omicron_VE_S,]$max_hosp_transition <-  max(omicron_hosp_transitions_notesting$phi)
  omicron_hosp_transitions_99 <- omicron_df[omicron_df$hosp_99 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==99 & df$VE==omicron_VE_S,]$min_hosp_transition <- min(omicron_hosp_transitions_99$phi)
  df[df$testing==99 & df$VE==omicron_VE_S,]$max_hosp_transition <- max(omicron_hosp_transitions_99$phi)
  omicron_hosp_transitions_50 <- omicron_df[omicron_df$hosp_50 >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==50 & df$VE==omicron_VE_S,]$min_hosp_transition <- min(omicron_hosp_transitions_50$phi)
  df[df$testing==50 & df$VE==omicron_VE_S,]$max_hosp_transition <- max(omicron_hosp_transitions_50$phi)
  omicron_hosp_transitions_biwk <- omicron_df[omicron_df$hosp_biwk >= 50,] %>%
    group_by(psi) %>% summarize(phi=min(phi))
  df[df$testing==299 & df$VE==omicron_VE_S,]$min_hosp_transition <- min(omicron_hosp_transitions_biwk$phi)
  df[df$testing==299 & df$VE==omicron_VE_S,]$max_hosp_transition <- max(omicron_hosp_transitions_biwk$phi)
  
  
  df$labels <- NA
  df[df$VE == waning_VE_S,]$labels <- "Waning (Low VE)"
  df[df$VE == baseline_VE_S,]$labels <- "Baseline VE"
  df[df$VE == boosted_VE_S,]$labels <- "Boosted (High VE)"
  df[df$VE == omicron_VE_S,]$labels <- "Omicron"
  
  # save final dataframe
  saveRDS(df,"df_Fig7_R06.RData")
  
}

#  Read in dataframes  ####
baseline_df <- readRDS("dataframes/df_Fig7_baseline_R06.RData")
waning_df <- readRDS("dataframes/df_Fig7_waning_R06.RData")
boosted_df <- readRDS("dataframes/df_Fig7_boosted_R06.RData")
omicron_df <- readRDS("dataframes/df_Fig7_omicron_R06.RData")
df <- readRDS("dataframes/df_Fig7_R06.RData")


#  Plot Figure 7  ####

p_inf_transition <- ggplot() +
  coord_flip() +
  geom_linerange(data = df[df$testing == 0,],
                 aes(x = VE, ymin = min_inf_transition*100, ymax = max_inf_transition*100), col = "black",
                 position = position_nudge(x = -0.025), size=1
  ) +
  geom_linerange(data = df[df$testing == 50,],
                 aes(x = VE, ymin = min_inf_transition*100, ymax = max_inf_transition*100), col = theta50_purple,
                 size=1
  ) +
  geom_linerange(data = df[df$testing == 99,],
                 aes(x = VE, ymin = min_inf_transition*100, ymax = max_inf_transition*100), col = theta99_purple,
                 position = position_nudge(x = 0.025), size=1
  ) +
  geom_linerange(data = df[df$testing == 299,],
                 aes(x = VE, ymin = min_inf_transition*100, ymax = max_inf_transition*100), col = thetabiwk_purple,
                 position = position_nudge(x = 0.05), size=1
  ) +
  # plot maxima
  geom_point(data = df[df$testing == 0,],
             aes(x = VE, y = max_inf_transition*100),
             position = position_nudge(x = -0.025),
             shape = 21, colour = "black", fill = "white", size = 2, stroke=1
  ) +
  geom_point(data = df[df$testing == 50,],
             aes(x = VE, y = max_inf_transition*100),
             shape = 21, colour = theta50_purple, fill = "white", size = 2, stroke=1
  ) +
  geom_point(data = df[df$testing == 99,],
             aes(x = VE, y = max_inf_transition*100),
             position = position_nudge(x = 0.025),
             shape = 21, colour = theta99_purple, fill = "white", size = 2, stroke=1
  ) +
  geom_point(data = df[df$testing == 299,],
             aes(x = VE, y = max_inf_transition*100),
             position = position_nudge(x = 0.05),
             shape = 21, colour = thetabiwk_purple, fill = "white", size = 2, stroke=1
  ) +
  # plot minima
  geom_point(data = df[df$testing == 0,],
             aes(x = VE, y = min_inf_transition*100), col = "black",
             position = position_nudge(x = -0.025), size=2
  ) +
  geom_point(data = df[df$testing == 50,],
             aes(x = VE, y = min_inf_transition*100), col = theta50_purple, size=2
  ) +
  geom_point(data = df[df$testing == 99,],
             aes(x = VE, y = min_inf_transition*100), col = theta99_purple,
             position = position_nudge(x = 0.025), size=2
  ) +
  geom_point(data = df[df$testing == 299,],
             aes(x = VE, y = min_inf_transition*100), col = thetabiwk_purple,
             position = position_nudge(x = 0.05), size=2
  ) +
  #scale_x_discrete("Waning","Baseline","Boosted") +
  ylab("Population vaccination rate (%)") +
  scale_y_continuous(expand = c(0, 0), limits = c(48, 100))  +
  #xlab("Vaccine Effectiveness") +
  ggtitle("Transition to majority\n breakthrough infections") +
  theme(legend.position = "none", panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black"))



p_trnsmsn_transition <- ggplot() +
  coord_flip() +
  geom_linerange(data = df[df$testing == 0,],
                 aes(x = VE, ymin = min_trnsmsn_transition*100, ymax = max_trnsmsn_transition*100), col = "black",
                 position = position_nudge(x = -0.025), size=1
  ) +
  geom_linerange(data = df[df$testing == 50,],
                 aes(x = VE, ymin = min_trnsmsn_transition*100, ymax = max_trnsmsn_transition*100), col = theta50_purple,
                 size=1
  ) +
  geom_linerange(data = df[df$testing == 99,],
                 aes(x = VE, ymin = min_trnsmsn_transition*100, ymax = max_trnsmsn_transition*100), col = theta99_purple,
                 position = position_nudge(x = 0.025), size=1
  ) +
  geom_linerange(data = df[df$testing == 299,],
                 aes(x = VE, ymin = min_trnsmsn_transition*100, ymax = max_trnsmsn_transition*100), col = thetabiwk_purple,
                 position = position_nudge(x = 0.05), size=1
  ) +
  # plot maxima
  geom_point(data = df[df$testing == 0,],
             aes(x = VE, y = max_trnsmsn_transition*100),
             position = position_nudge(x = -0.025),
             shape = 21, colour = "black", fill = "white", size = 2, stroke=1
  ) +
  geom_point(data = df[df$testing == 50,],
             aes(x = VE, y = max_trnsmsn_transition*100),
             shape = 21, colour = theta50_purple, fill = "white", size = 2, stroke=1
  ) +
  geom_point(data = df[df$testing == 99,],
             aes(x = VE, y = max_trnsmsn_transition*100),
             position = position_nudge(x = 0.025),
             shape = 21, colour = theta99_purple, fill = "white", size = 2, stroke=1
  ) +
  geom_point(data = df[df$testing == 299,],
             aes(x = VE, y = max_trnsmsn_transition*100),
             position = position_nudge(x = 0.05),
             shape = 21, colour = thetabiwk_purple, fill = "white", size = 2, stroke=1
  ) +
  # plot minima
  geom_point(data = df[df$testing == 0,],
             aes(x = VE, y = min_trnsmsn_transition*100), col = "black",
             position = position_nudge(x = -0.025), size=2
  ) +
  geom_point(data = df[df$testing == 50,],
             aes(x = VE, y = min_trnsmsn_transition*100), col = theta50_purple, size=2
  ) +
  geom_point(data = df[df$testing == 99,],
             aes(x = VE, y = min_trnsmsn_transition*100), col = theta99_purple,
             position = position_nudge(x = 0.025), size=2
  ) +
  geom_point(data = df[df$testing == 299,],
             aes(x = VE, y = min_trnsmsn_transition*100), col = thetabiwk_purple,
             position = position_nudge(x = 0.05), size=2
  ) +
  #scale_x_discrete("Waning","Baseline","Boosted") +
  ylab("Population vaccination rate (%)") +
  scale_y_continuous(expand = c(0, 0), limits = c(48, 100))  +
  #xlab("Vaccine Effectiveness") +
  ggtitle("Transition to majority\n breakthrough transmission") +
  onlyx_theme +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"))


p_hosp_transition <- ggplot() +
  coord_flip() +
  geom_linerange(data = df[df$testing == 0,],
                 aes(x = VE, ymin = min_hosp_transition*100, ymax = max_hosp_transition*100), col = "black",
                 position = position_nudge(x = -0.025), size=1
  ) +
  geom_linerange(data = df[df$testing == 50,],
                 aes(x = VE, ymin = min_hosp_transition*100, ymax = max_hosp_transition*100), col = theta50_purple,
                 size=1
  ) +
  geom_linerange(data = df[df$testing == 99,],
                 aes(x = VE, ymin = min_hosp_transition*100, ymax = max_hosp_transition*100), col = theta99_purple,
                 position = position_nudge(x = 0.025), size=1
  ) +
  geom_linerange(data = df[df$testing == 299,],
                 aes(x = VE, ymin = min_hosp_transition*100, ymax = max_hosp_transition*100), col = thetabiwk_purple,
                 position = position_nudge(x = 0.05), size=1
  ) +
  # plot maxima
  geom_point(data = df[df$testing == 0,],
             aes(x = VE, y = max_hosp_transition*100),
             position = position_nudge(x = -0.025),
             shape = 21, colour = "black", fill = "white", size = 2, stroke=1
  ) +
  geom_point(data = df[df$testing == 50,],
             aes(x = VE, y = max_hosp_transition*100),
             shape = 21, colour = theta50_purple, fill = "white", size = 2, stroke=1
  ) +
  geom_point(data = df[df$testing == 99,],
             aes(x = VE, y = max_hosp_transition*100),
             position = position_nudge(x = 0.025),
             shape = 21, colour = theta99_purple, fill = "white", size = 2, stroke=1
  ) +
  geom_point(data = df[df$testing == 299,],
             aes(x = VE, y = max_hosp_transition*100),
             position = position_nudge(x = 0.05),
             shape = 21, colour = thetabiwk_purple, fill = "white", size = 2, stroke=1
  ) +
  # plot minima
  geom_point(data = df[df$testing == 0,],
             aes(x = VE, y = min_hosp_transition*100), col = "black",
             position = position_nudge(x = -0.025), size=2
  ) +
  geom_point(data = df[df$testing == 50,],
             aes(x = VE, y = min_hosp_transition*100), col = theta50_purple, size=2
  ) +
  geom_point(data = df[df$testing == 99,],
             aes(x = VE, y = min_hosp_transition*100), col = theta99_purple,
             position = position_nudge(x = 0.025), size=2
  ) +
  geom_point(data = df[df$testing == 299,],
             aes(x = VE, y = min_hosp_transition*100), col = thetabiwk_purple,
             position = position_nudge(x = 0.05), size=2
  ) +
  #scale_x_discrete("Waning","Baseline","Boosted") +
  ylab("Population vaccination rate (%)") +
  scale_y_continuous(expand = c(0, 0), limits = c(48, 102))  +
  #xlab("Vaccine Effectiveness") +
  ggtitle("Transition to majority\n breakthrough hospitalizations") +
  onlyx_theme +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(), axis.line = element_line(colour = "black"))


suppfig7 <- ggarrange(p_inf_transition, NULL, p_trnsmsn_transition, NULL, p_hosp_transition, NULL,
                  widths = c(1.02, -0.05, 1, -0.05, 1, 0.1),
                  labels = c('   a ', NA, '  b', NA, '  c', NA),
                  ncol = 6,
                  label.y = 0.96,
                  align = "hv")
suppfig7

ggsave("SuppFig7.pdf", suppfig7, device = cairo_pdf, width = 7.5, height = 3.5)