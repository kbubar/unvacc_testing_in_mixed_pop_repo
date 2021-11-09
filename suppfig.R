source("setup.R")


# _____________________________________________________________________
# SUPP FIG1 - infections with no testing ####
# _____________________________________________________________________

VE_I_vec <- c(0, 0.3, 0.6)
this_theta <- 0
this_q <- 0

for (VE_I in VE_I_vec){
  #* Top row - total infections and breakthrough cases over phi ####
  #* # Same as Fig1C
  df <- data.frame(phi = phi_vec)
  df$tot_infections <- sapply(phi_vec, compute_tot_infections, 
                              VE_I = this_VE_I, VE_S = this_VE_S, 
                              this_theta, q = this_q,
                              psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff <- sapply(phi_vec, compute_Reff,
                    VE_I = this_VE_I, VE_S = this_VE_S, 
                    this_theta, q = this_q,
                    psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  df$breakthrough_withext <- sapply(phi_vec, compute_percent_v_infections_withext, 
                                    VE_I = this_VE_I, VE_S = this_VE_S, 
                                    this_theta, q = this_q,
                                    psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  row1 <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y=tot_infections/N*100), col = myblack, size = 1) + 
    geom_line(aes(y=breakthrough_withext), col = mylightgray, size = 1) +
    ylab("Total infected (%)") + 
    xlab("") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100))
  
  Reff_1 <- min(which(df$Reff <= 1))  
  
  if (Reff_1 <= 100){
    row1 <-  row1 + geom_vline(xintercept = Reff_1, alpha = 0.5, linetype = "dashed", size = 0.5) 
  }

  #* Bottom row - cumulative transmission mode over phi ####
  #* Same as FIG1E
  df <- data.frame(phi = phi_vec)
  
  lists_who_caused <- lapply(phi_vec, compute_who_caused_cases_tot, 
                             VE_I = this_VE_I, VE_S = this_VE_S, 
                             this_theta, q = this_q,
                             psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  mat_who_caused <- matrix(unlist(lists_who_caused), ncol=6, byrow=TRUE)
  
  df$cases_in_v_by_ext <- mat_who_caused[,5]
  df$cases_in_u_by_ext <- mat_who_caused[,6]
  df$cases_in_v_by_v <- mat_who_caused[,1]
  df$cases_in_u_by_v <- mat_who_caused[,3]
  df$cases_in_v_by_u <- mat_who_caused[,2]
  df$cases_in_u_by_u <- mat_who_caused[,4]
  
  df_toplot <- melt(df, id = c("phi"))
  
  row2 <- ggplot(df_toplot, aes(x=phi*100, y=value*100, color = variable)) + 
    geom_line(size = 1) +
    ylab("Transmission mode (%)") +
    xlab("") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100.1)) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_color_manual(values = c(mygray, mylightgray, mydarkteal, mylightteal, mylightorange,
                                  mydarkorange))
  
  if (Reff_1 <= 100){
    row2 <- row2 + geom_vline(xintercept = Reff_1, alpha = 0.5, linetype = "dashed", size = 0.5)
  }
  
  
  if (VE_I == 0){
    A <- row1 + scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
      labs(title = VE[I] ~ "= 0%") +
      theme(axis.title.y.left = element_text(color = myblack),
            axis.title.x = element_blank(),
            axis.text.x = element_blank())

    D <- row2 +
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_text(family = "Arial"))
    
  } else if (VE_I == 0.3){
    B <- row1 + scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
      labs(title = VE[I] ~ "= 30%") +
      theme(axis.title.y.left = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank())
    
    E <- row2 + 
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank())
  } else {
    C <- row1 + scale_y_continuous(expand = c(0, 0), limits = c(0, 100),
                                sec.axis = sec_axis(~., name="Breakthrough infections (%)")) +
      labs(title = VE[I] ~ "= 60%") +
      theme(axis.title.y.left = element_blank(),
            axis.title.y.right = element_text(color = mylightgray),
            axis.text.y.left = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank())
    
    F <- row2 + 
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank())
  }
}

# Code for Supp fig. export 1000x700
suppfig1 <- ggarrange(A, NULL, B, NULL, C, 
          NULL, NULL, NULL, NULL, NULL,
          D, NULL, E, NULL, F, 
          nrow = 3, 
          ncol = 5, 
          widths = c(1, -0.3, 1, -0.3, 1),
          heights = c(1, -0.15, 1),
          align = "hv",
          labels = c("a", NA, "     b", NA, "     c", 
                     rep(NA, 5),
                     "d", NA, "     e", NA, "      f", NA),
          label.y = c(rep(0.92, 5), rep(NA, 5), rep(0.92, 5)))

annotate_figure(suppfig1,
                bottom = text_grob("Population vaccination rate (%)", size = 14, family = "Arial",
                                   vjust = -0.1))

ggsave("suppfig1.pdf", device = cairo_pdf, width = 8, height = 5.5)

# _____________________________________________________________________
# SUPP FIG2 - w/testing ####
# _____________________________________________________________________
this_q <- 0
panels <- c(1,2,3)

for (this_panel in panels){
  R0 <- this_panel*2
  alpha <- R0*gamma/N # transmissibility
  
  R0_ideal_theta <- alpha*N*(1-ideal_theta)/gamma 
  R0_mod_theta <- alpha*N*(1-mod_theta)/gamma 
  R0_real_theta <- alpha*N*(1-real_theta)/gamma 
  
  df <- data.frame(phi = phi_vec)
  # Total infections with various testing scenarios
  df$tot_infections_notesting <- sapply(phi_vec, compute_tot_infections, 
                                        VE_I = this_VE_I, VE_S = this_VE_S, 
                                        theta = 0, q = this_q,
                                        psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$tot_infections_idealtesting <- sapply(phi_vec, compute_tot_infections, 
                                           VE_I = this_VE_I, VE_S = this_VE_S, 
                                           theta = ideal_theta, q = this_q,
                                           psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$tot_infections_modtesting <- sapply(phi_vec, compute_tot_infections, 
                                         VE_I = this_VE_I, VE_S = this_VE_S, 
                                         theta = mod_theta, q = this_q,
                                         psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$tot_infections_realtesting <- sapply(phi_vec, compute_tot_infections, 
                                          VE_I = this_VE_I, VE_S = this_VE_S, 
                                          theta = real_theta, q = this_q,
                                          psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  # Total number of cases averted 
  df$cases_averted_ideal <- (df$tot_infections_notesting - df$tot_infections_idealtesting)
  df$cases_averted_mod <- (df$tot_infections_notesting - df$tot_infections_modtesting)
  df$cases_averted_real <- (df$tot_infections_notesting - df$tot_infections_realtesting)
  
  # Proportion of infections averted in unvax class
  df$u_infections_notesting <- sapply(phi_vec, compute_u_infections, 
                                      VE_I = this_VE_I, VE_S = this_VE_S, 
                                      theta = 0, q = this_q,
                                      psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$u_infections_idealtesting <- sapply(phi_vec, compute_u_infections, 
                                         VE_I = this_VE_I, VE_S = this_VE_S, 
                                         theta = ideal_theta, q = this_q,
                                         psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$u_infections_modtesting <- sapply(phi_vec, compute_u_infections, 
                                       VE_I = this_VE_I, VE_S = this_VE_S, 
                                       theta = mod_theta, q = this_q,
                                       psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$u_infections_realtesting <- sapply(phi_vec, compute_u_infections, 
                                        VE_I = this_VE_I, VE_S = this_VE_S, 
                                        theta = real_theta, q = this_q,
                                        psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  df$prop_cases_averted_u_ideal <- (df$u_infections_notesting - df$u_infections_idealtesting) / df$cases_averted_ideal * 100
  df$prop_cases_averted_u_mod <- (df$u_infections_notesting - df$u_infections_modtesting) / df$cases_averted_mod * 100
  df$prop_cases_averted_u_real <- (df$u_infections_notesting - df$u_infections_realtesting) / df$cases_averted_real * 100
  
  
  df$num_cases_averted_u_ideal <- (df$u_infections_notesting - df$u_infections_idealtesting) 
  df$num_cases_averted_u_mod <- (df$u_infections_notesting - df$u_infections_modtesting) 
  df$num_cases_averted_u_real <- (df$u_infections_notesting - df$u_infections_realtesting) 
  
  df$num_cases_averted_v_ideal <- (df$cases_averted_ideal - df$num_cases_averted_u_ideal) 
  df$num_cases_averted_v_mod <- (df$cases_averted_mod - df$num_cases_averted_u_mod) 
  df$num_cases_averted_v_real <- (df$cases_averted_real - df$num_cases_averted_u_real) 
  
  # Reff
  df$Reff_notesting <- sapply(phi_vec, compute_Reff, 
                              VE_I = this_VE_I, VE_S = this_VE_S, 
                              theta = 0, q = this_q,
                              psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_idealtesting <- sapply(phi_vec, compute_Reff, 
                                 VE_I = this_VE_I, VE_S = this_VE_S, 
                                 theta = ideal_theta, q = this_q,
                                 psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_modtesting <- sapply(phi_vec, compute_Reff, 
                               VE_I = this_VE_I, VE_S = this_VE_S, 
                               theta = mod_theta, q = this_q,
                               psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_realtesting <- sapply(phi_vec, compute_Reff, 
                                VE_I = this_VE_I, VE_S = this_VE_S, 
                                theta = real_theta, q = this_q,
                                psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  # Number of tests 
  df$numtests_idealtesting <- sapply(phi_vec, compute_num_tests, 
                                     VE_I = this_VE_I, VE_S = this_VE_S,  
                                     theta = ideal_theta, freq = high_freq, 
                                     inf_period = 1/gamma, compliance = high_compliance, q = this_q,
                                     psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$numtests_modtesting <- sapply(phi_vec, compute_num_tests, 
                                   VE_I = this_VE_I, VE_S = this_VE_S, 
                                   theta = mod_theta, freq = low_freq, 
                                   inf_period = 1/gamma, compliance = high_compliance, q = this_q,
                                   psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$numtests_realtesting <- sapply(phi_vec, compute_num_tests, 
                                    VE_I = this_VE_I, VE_S = this_VE_S, 
                                    theta = real_theta, freq = low_freq, 
                                    inf_period = 1/gamma, compliance = low_compliance, q = this_q,
                                    psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  # Reduction per test
  df$reducpertest_idealtesting <- df$cases_averted_ideal / df$numtests_idealtesting
  df$reducpertest_modtesting <- df$cases_averted_mod / df$numtests_modtesting
  df$reducpertest_realtesting <- df$cases_averted_real / df$numtests_realtesting
  df$reducpertest_notesting <- 0
  
  # fix anywhere that had NaN because 0 in the denominator
  this_elem <- which(is.na(df[101,])) 
  df[101,this_elem] <- 0
  
  A <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y = tot_infections_idealtesting/N*100), col = mygreen, size = my_linesize)+
    geom_line(aes(y = tot_infections_notesting/N*100), col = myred, size = my_linesize) +  
    geom_line(aes(y = tot_infections_modtesting/N*100), col = myblue, size = my_linesize) + 
    geom_line(aes(y = tot_infections_realtesting/N*100), col = myyellow, size = my_linesize) + 
    ylab("") + # Total infections (%)
    xlab("") +
    #onlyy_theme +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) 
  
  B <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y=cases_averted_ideal/N*100), col = mygreen, size = my_linesize) +
    geom_line(aes(y=cases_averted_mod/N*100), col = myblue, size = my_linesize) +
    geom_line(aes(y = cases_averted_real/N*100), col = myyellow, size = my_linesize) + 
    geom_line(aes(y = 0), col = myred, size = my_linesize) +  
    ylab("") + # Cases averted (#)
    xlab("") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-1, 100))  
  
  C <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y=cases_averted_ideal/tot_infections_notesting*100), col = mygreen, size = my_linesize) +
    geom_line(aes(y=cases_averted_mod/tot_infections_notesting*100), col = myblue, size = my_linesize) +
    geom_line(aes(y=cases_averted_real/tot_infections_notesting*100), col = myyellow, size = my_linesize) +
    geom_line(aes(y=0), col = myred, size = my_linesize) +
    ylab("") + # % reduction in cases\n(rel. to no testing)
    xlab("") +
    #onlyy_theme +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(-1, 100)) 
  
  D <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y=prop_cases_averted_u_ideal), col = mygreen, size = my_linesize) +
    geom_line(aes(y=prop_cases_averted_u_mod), col = myblue, size = my_linesize) +
    geom_line(aes(y=prop_cases_averted_u_real), col = myyellow, size = my_linesize) +
    geom_line(aes(y=0), col = myred, size = my_linesize) +
    ylab("") + # % of cases averted in unvax population
    xlab("") +
    #onlyy_theme +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(-1, 100)) 
  
  E <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y=numtests_idealtesting/10^6), col = mygreen, size = my_linesize) +
    geom_line(aes(y=numtests_modtesting/10^6), col = myblue, size = my_linesize) +
    geom_line(aes(y = numtests_realtesting/10^6), col = myyellow, size = my_linesize) + 
    geom_line(aes(y = 0), col = myred, size = my_linesize) +  
    ylab("") + # Number of Tests (#)
    xlab("") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-0.03, 1.5))  
  
  F <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y=reducpertest_idealtesting * 100), col = mygreen, size = my_linesize) +
    geom_line(aes(y=reducpertest_modtesting * 100), col = myblue, size = my_linesize) +
    geom_line(aes(y = reducpertest_realtesting * 100), col = myyellow, size = my_linesize) +
    geom_line(aes(y = 0), col = myred, size = my_linesize) +
    ylab("") + # Cases averted per 100 tests (#)
    xlab("") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-0.05, 6))
  
  G <- ggplot(df, aes(x=phi*100)) +
    geom_hline(yintercept = 1, size = 0.5, alpha = 0.5, linetype = "dashed") +
    geom_line(aes(y = Reff_notesting), col = myred, size = my_linesize) +
    geom_line(aes(y = Reff_modtesting), col = myblue, size = my_linesize) +
    geom_line(aes(y = Reff_idealtesting), col = mygreen, size = my_linesize) +
    geom_line(aes(y = Reff_realtesting), col = myyellow, size = my_linesize) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 6)) + 
    ylab("") + # R_eff 
    #onlyy_theme +
    xlab("") 
  
  if (this_panel == 1){
    A <- A + onlyy_theme + 
      ggtitle("Total infections\n(% of pop.)")
    B <- B + nolabels_theme + 
      ggtitle("Infections averted\nrel. to no testing\n(% of pop.)")
    C <- C + nolabels_theme + 
      ggtitle("Infections averted\n(% of tot.\ninfections)")
    D <- D + nolabels_theme + 
      ggtitle("Composition of\ninfections averted\n(% unvacc.)")
    E <- E + onlyy_theme + 
      ggtitle(" Total tests\nadministered\n(millions)")
    F <- F + onlyy_theme + 
      ggtitle("Infections averted\nper 100 tests")
    G <- G + nolabels_theme + 
      ggtitle(expression(R[eff]))
    
    panel1sup <- ggarrange(A, NULL, B, NULL, C, NULL, D, NULL, E, NULL, F, NULL, G, NULL,
                           nrow = 1,
                           widths = c(1, -0.15, 1, -0.15, 1, -0.15, 1, -0.1, 1, -0.1, 1, -0.15, 1, 0.1), 
                           align = "hv",
                           labels = c('a', NA, 'b', NA, 'c', NA,'d', NA, 'e', NA, ' f', NA, 'g', NA),
                           label.y = 0.78,
                           label.x =  c(0, rep(c(NA, 0.17),3), NA, 0.08, NA, 0.08, NA, 0.17, NA))
    
  } else if (this_panel == 2){
    A <- A + onlyy_theme
    B <- B + nolabels_theme 
    C <- C + nolabels_theme 
    D <- D + nolabels_theme 
    E <- E + onlyy_theme 
    F <- F + onlyy_theme 
    G <- G + nolabels_theme 
    
    panel2sup <- ggarrange(A, NULL, B, NULL, C, NULL, D, NULL, E, NULL, F, NULL, G, NULL,
                           nrow = 1,
                           widths = c(1, -0.15, 1, -0.15, 1, -0.15, 1, -0.1, 1, -0.1, 1, -0.15, 1, 0.1),  
                           align = "hv",
                           labels = c('h', NA, ' i', NA, ' j', NA, 'k', NA, 'l', NA, 'm', NA, 'n', NA),
                           label.x =  c(0, rep(c(NA, 0.17),3), NA, 0.08, NA, 0.08, NA, 0.17, NA))
  } else {
    B <- B + onlyx_theme 
    C <- C + onlyx_theme 
    D <- D + onlyx_theme
    G <- G + onlyx_theme
    
    panel3sup <- ggarrange(A, NULL, B, NULL, C, NULL, D, NULL, E, NULL, F, NULL, G, NULL,
                           nrow = 1,
                           widths = c(1, -0.15, 1, -0.15, 1, -0.15, 1, -0.1, 1, -0.1, 1, -0.15, 1, 0.1),  
                           align = "hv",
                           labels = c('o', NA, 'p', NA, 'q', NA, 'r', NA, 's', NA,' t', NA,'u', NA),
                           label.x =  c(0, rep(c(NA, 0.17),3), NA, 0.08, NA, 0.08, NA, 0.17, NA))
  }
}

# supplementary figure
suppfig2 <- ggarrange(panel1sup, panel2sup, panel3sup, 
          nrow = 3,
          heights = c(1.3, 1, 1.1),
          align = "hv")

annotate_figure(suppfig2,
                bottom = text_grob("Population vaccination rate (%)", size = 14, family = "Arial",
                                   vjust = -0.1))

ggsave("suppfig2.pdf", device = cairo_pdf, width = 14, height = 8)



# _____________________________________________________________________
# SUPP FIG3 - w/homophily ####
# _____________________________________________________________________
this_phi <- 0.55
panels <- c(1,2,3)
this_theta <- 0

for (this_panel in panels){
  R0 <- this_panel*2
  alpha <- R0*gamma/N # transmissibility
  
  #*
  #* Panel A - transmission modes over time, phi = 55%, q = 0, 0.8 ####
  #* 
  df <- data.frame(time = t)
  
  list_who_caused <- compute_who_caused_daily_infections(this_phi, VE_I = this_VE_I, VE_S = this_VE_S, 
                                                         this_theta, q = q0,
                                                         psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$cases_in_v_by_v_q0 <- unlist(list_who_caused[[1]])
  df$cases_in_v_by_u_q0 <- unlist(list_who_caused[[2]])
  df$cases_in_u_by_v_q0 <- unlist(list_who_caused[[3]])
  df$cases_in_u_by_u_q0 <- unlist(list_who_caused[[4]])
  
  list_who_caused <- compute_who_caused_daily_infections(this_phi, VE_I = this_VE_I, VE_S = this_VE_S, 
                                                         this_theta, q = qhigh,
                                                         psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$cases_in_v_by_v_qhigh <- unlist(list_who_caused[[1]])
  df$cases_in_v_by_u_qhigh <- unlist(list_who_caused[[2]])
  df$cases_in_u_by_v_qhigh <- unlist(list_who_caused[[3]])
  df$cases_in_u_by_u_qhigh <- unlist(list_who_caused[[4]])
  
  A <- ggplot(df, aes(x = time)) + 
    geom_line(aes(y = cases_in_u_by_v_q0), col = mylightteal, size = 0.8, alpha = 0.6, linetype = "dashed") + 
    geom_line(aes(y = cases_in_v_by_u_q0), col = mylightorange, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = cases_in_u_by_u_q0), col = mydarkorange, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = cases_in_v_by_v_q0), col = mydarkteal, size = 0.8, alpha = 0.6, linetype = "dashed") + 
    
    geom_line(aes(y = cases_in_u_by_v_qhigh), col = mylightteal, size = my_linesize) + 
    geom_line(aes(y = cases_in_v_by_u_qhigh), col = mylightorange, size = my_linesize) +
    geom_line(aes(y = cases_in_u_by_u_qhigh), col = mydarkorange, size = my_linesize) +
    geom_line(aes(y = cases_in_v_by_v_qhigh), col = mydarkteal, size = my_linesize) + 
    ylab("")+
    #ggtitle("New daily infections (#)") + 
    xlab("")+ #("Time (days)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 200)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 550)) +
    alllabels_theme
  #onlyy_theme
  
  #*
  #* Panel B - line plot of transmission modes over phi with q = 0.8 ####
  #* 
  df <- data.frame(phi = phi_vec)
  
  lists_who_caused <- lapply(phi_vec, compute_who_caused_cases_tot, 
                             VE_I = this_VE_I, VE_S = this_VE_S, 
                             this_theta, q = q0,
                             psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  mat_who_caused <- matrix(unlist(lists_who_caused), ncol=6, byrow=TRUE)
  
  df$cases_in_v_by_ext <- mat_who_caused[,5]
  df$cases_in_u_by_ext <- mat_who_caused[,6]
  df$cases_in_v_by_v <- mat_who_caused[,1]
  df$cases_in_u_by_v <- mat_who_caused[,3]
  df$cases_in_v_by_u <- mat_who_caused[,2]
  df$cases_in_u_by_u <- mat_who_caused[,4]
  
  lists_who_caused_qhigh <- lapply(phi_vec, compute_who_caused_cases_tot, 
                                   VE_I = this_VE_I, VE_S = this_VE_S, 
                                   this_theta, q = qhigh,
                                   psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  mat_who_caused_qhigh <- matrix(unlist(lists_who_caused_qhigh), ncol=6, byrow=TRUE)
  
  df$cases_in_v_by_ext_qhigh <- mat_who_caused_qhigh[,5]
  df$cases_in_u_by_ext_qhigh <- mat_who_caused_qhigh[,6]
  df$cases_in_v_by_v_qhigh <- mat_who_caused_qhigh[,1]
  df$cases_in_u_by_v_qhigh <- mat_who_caused_qhigh[,3]
  df$cases_in_v_by_u_qhigh <- mat_who_caused_qhigh[,2]
  df$cases_in_u_by_u_qhigh <- mat_who_caused_qhigh[,4]
  
  B <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y = cases_in_u_by_v*100), col = mylightteal, size = 0.8, alpha = 0.6, linetype = "dashed") + 
    geom_line(aes(y = cases_in_v_by_u*100), col = mylightorange, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = cases_in_u_by_u*100), col = mydarkorange, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = cases_in_v_by_v*100), col = mydarkteal, size = 0.8, alpha = 0.6, linetype = "dashed") + 
    
    geom_line(aes(y = cases_in_u_by_v_qhigh*100), col = mylightteal, size = my_linesize) +
    geom_line(aes(y = cases_in_v_by_u_qhigh*100), col = mylightorange, size = my_linesize) +
    geom_line(aes(y = cases_in_u_by_u_qhigh*100), col = mydarkorange, size = my_linesize) +
    geom_line(aes(y = cases_in_v_by_v_qhigh*100), col = mydarkteal, size = my_linesize) +
    ylab("") + # Transmission mode (%)
    xlab("") + # Population vaccination rate (%)
    labs(fill = "") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100.1)) +
    scale_x_continuous(expand = c(0, 0), breaks = c(0, 50, 100)) + 
    scale_color_manual(values = c(mygray, mylightgray, mydarkteal, mylightteal, mylightorange,
                                  mydarkorange)) +
    #labels = c("v to v", "v to u", "u to v", "u to u")) + 
    theme(legend.position = "none")
  
  #*
  #* Panel C - infections averted vs phi, q = 0, 0.5 ####
  #* 
  df <- data.frame(phi = phi_vec)
  df$tot_infections_notesting_q0 <- sapply(phi_vec, compute_tot_infections, 
                                           VE_I = this_VE_I, VE_S = this_VE_S, 
                                           theta = 0, q = q0,
                                           psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$tot_infections_ideal_q0 <- sapply(phi_vec, compute_tot_infections, 
                                       VE_I = this_VE_I, VE_S = this_VE_S, 
                                       theta = ideal_theta, q = q0,
                                       psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$tot_infections_mod_q0 <- sapply(phi_vec, compute_tot_infections, 
                                     VE_I = this_VE_I, VE_S = this_VE_S, 
                                     theta = mod_theta, q = q0,
                                     psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$tot_infections_real_q0 <- sapply(phi_vec, compute_tot_infections, 
                                      VE_I = this_VE_I, VE_S = this_VE_S, 
                                      theta = real_theta, q = q0,
                                      psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  df$tot_infections_notesting_qhigh <- sapply(phi_vec, compute_tot_infections, 
                                              VE_I = this_VE_I, VE_S = this_VE_S, 
                                              theta = 0, q = qhigh,
                                              psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$tot_infections_ideal_qhigh <- sapply(phi_vec, compute_tot_infections, 
                                          VE_I = this_VE_I, VE_S = this_VE_S, 
                                          theta = ideal_theta, q = qhigh,
                                          psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$tot_infections_mod_qhigh <- sapply(phi_vec, compute_tot_infections, 
                                        VE_I = this_VE_I, VE_S = this_VE_S, 
                                        theta = mod_theta, q = qhigh,
                                        psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$tot_infections_real_qhigh <- sapply(phi_vec, compute_tot_infections, 
                                         VE_I = this_VE_I, VE_S = this_VE_S, 
                                         theta = real_theta, q = qhigh,
                                         psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  df$cases_averted_ideal_q0 <- (df$tot_infections_notesting_q0 - df$tot_infections_ideal_q0)
  df$cases_averted_mod_q0 <- (df$tot_infections_notesting_q0 - df$tot_infections_mod_q0)
  df$cases_averted_real_q0 <- (df$tot_infections_notesting_q0 - df$tot_infections_real_q0)
  
  df$cases_averted_ideal_qhigh <- (df$tot_infections_notesting_qhigh - df$tot_infections_ideal_qhigh)
  df$cases_averted_mod_qhigh <- (df$tot_infections_notesting_qhigh - df$tot_infections_mod_qhigh)
  df$cases_averted_real_qhigh <- (df$tot_infections_notesting_qhigh - df$tot_infections_real_qhigh)
  
  C <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y=cases_averted_ideal_q0/N*100), col = mygreen, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y=cases_averted_mod_q0/N*100), col = myblue, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = cases_averted_real_q0/N*100), col = myyellow, size = 0.8, alpha = 0.6, linetype = "dashed") + 
    geom_line(aes(y = 0), col = myred, size = my_linesize) +  
    geom_line(aes(y=cases_averted_ideal_qhigh/N*100), col = mygreen, size = my_linesize) +
    geom_line(aes(y=cases_averted_mod_qhigh/N*100), col = myblue, size = my_linesize) +
    geom_line(aes(y = cases_averted_real_qhigh/N*100), col = myyellow, size = my_linesize) +
    ylab("") + # Cases averted (#)
    xlab("") +
    #ggtitle("Infections averted")+
    #onlyy_theme +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-1, 100))  
  
  
  #*
  #* Panel D - cases averted - % unvax, q = 0, 0.8 ####
  #* 
  df$u_infections_notesting_q0 <- sapply(phi_vec, compute_u_infections, 
                                         VE_I = this_VE_I, VE_S = this_VE_S, 
                                         theta = 0, q = q0,
                                         psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$u_infections_idealtesting_q0 <- sapply(phi_vec, compute_u_infections, 
                                            VE_I = this_VE_I, VE_S = this_VE_S, 
                                            theta = ideal_theta, q = q0,
                                            psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$u_infections_modtesting_q0 <- sapply(phi_vec, compute_u_infections, 
                                          VE_I = this_VE_I, VE_S = this_VE_S, 
                                          theta = mod_theta, q = q0,
                                          psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$u_infections_realtesting_q0 <- sapply(phi_vec, compute_u_infections, 
                                           VE_I = this_VE_I, VE_S = this_VE_S, 
                                           theta = real_theta, q = q0,
                                           psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  df$prop_cases_averted_u_ideal_q0 <- (df$u_infections_notesting_q0 - df$u_infections_idealtesting_q0) / df$cases_averted_ideal_q0 * 100
  df$prop_cases_averted_u_mod_q0 <- (df$u_infections_notesting_q0 - df$u_infections_modtesting_q0) / df$cases_averted_mod_q0 * 100
  df$prop_cases_averted_u_real_q0 <- (df$u_infections_notesting_q0 - df$u_infections_realtesting_q0) / df$cases_averted_real_q0 * 100
  
  df$num_cases_averted_u_ideal_q0 <- (df$u_infections_notesting_q0 - df$u_infections_idealtesting_q0) 
  df$num_cases_averted_u_mod_q0 <- (df$u_infections_notesting_q0 - df$u_infections_modtesting_q0) 
  df$num_cases_averted_u_real_q0 <- (df$u_infections_notesting_q0 - df$u_infections_realtesting_q0) 
  df$num_cases_averted_v_ideal_q0 <- (df$cases_averted_ideal_q0 - df$num_cases_averted_u_ideal_q0) 
  df$num_cases_averted_v_mod_q0 <- (df$cases_averted_mod_q0 - df$num_cases_averted_u_mod_q0) 
  df$num_cases_averted_v_real_q0 <- (df$cases_averted_real_q0 - df$num_cases_averted_u_real_q0)
  
  df$u_infections_notesting_qhigh <- sapply(phi_vec, compute_u_infections, 
                                         VE_I = this_VE_I, VE_S = this_VE_S, 
                                         theta = 0, q = qhigh,
                                         psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$u_infections_idealtesting_qhigh <- sapply(phi_vec, compute_u_infections, 
                                            VE_I = this_VE_I, VE_S = this_VE_S, 
                                            theta = ideal_theta, q = qhigh,
                                            psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$u_infections_modtesting_qhigh <- sapply(phi_vec, compute_u_infections, 
                                          VE_I = this_VE_I, VE_S = this_VE_S, 
                                          theta = mod_theta, q = qhigh,
                                          psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$u_infections_realtesting_qhigh <- sapply(phi_vec, compute_u_infections, 
                                           VE_I = this_VE_I, VE_S = this_VE_S, 
                                           theta = real_theta, q = qhigh,
                                           psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  df$prop_cases_averted_u_ideal_qhigh <- (df$u_infections_notesting_qhigh - df$u_infections_idealtesting_qhigh) / df$cases_averted_ideal_qhigh * 100
  df$prop_cases_averted_u_mod_qhigh <- (df$u_infections_notesting_qhigh - df$u_infections_modtesting_qhigh) / df$cases_averted_mod_qhigh * 100
  df$prop_cases_averted_u_real_qhigh <- (df$u_infections_notesting_qhigh - df$u_infections_realtesting_qhigh) / df$cases_averted_real_qhigh * 100
  
  df$num_cases_averted_u_ideal_qhigh <- (df$u_infections_notesting_qhigh - df$u_infections_idealtesting_qhigh) 
  df$num_cases_averted_u_mod_qhigh <- (df$u_infections_notesting_qhigh - df$u_infections_modtesting_qhigh) 
  df$num_cases_averted_u_real_qhigh <- (df$u_infections_notesting_qhigh - df$u_infections_realtesting_qhigh) 
  df$num_cases_averted_v_ideal_qhigh <- (df$cases_averted_ideal_qhigh - df$num_cases_averted_u_ideal_qhigh) 
  df$num_cases_averted_v_mod_qhigh <- (df$cases_averted_mod_qhigh - df$num_cases_averted_u_mod_qhigh) 
  df$num_cases_averted_v_real_qhigh <- (df$cases_averted_real_qhigh - df$num_cases_averted_u_real_qhigh)
  
  D <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y=prop_cases_averted_u_ideal_q0), col = mygreen, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y=prop_cases_averted_u_mod_q0), col = myblue, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y=prop_cases_averted_u_real_q0), col = myyellow, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y=prop_cases_averted_u_ideal_qhigh), col = mygreen, size = my_linesize) +
    geom_line(aes(y=prop_cases_averted_u_mod_qhigh), col = myblue, size = my_linesize) +
    geom_line(aes(y=prop_cases_averted_u_real_qhigh), col = myyellow, size = my_linesize) +
    geom_line(aes(y=0), col = myred, size = my_linesize) +
    ylab("") + # % of cases averted in unvax population
    xlab("") +
    #onlyy_theme +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(-1, 100)) 
  
  #*
  #* Panel E - cases averted per 100 tests, q = 0, 0.8 ####
  #* 
  df$numtests_ideal_q0 <- sapply(phi_vec, compute_num_tests, 
                                 this_VE_I, this_VE_S,
                                 theta = ideal_theta, freq = high_freq,
                                 inf_period = 1/gamma, compliance = high_compliance, q = q0,
                                 psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$numtests_mod_q0 <- sapply(phi_vec, compute_num_tests,
                               this_VE_I, this_VE_S,
                               theta = mod_theta, freq = low_freq,
                               inf_period = 1/gamma, compliance = high_compliance, q = q0,
                               psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$numtests_real_q0 <- sapply(phi_vec, compute_num_tests, 
                                this_VE_I, this_VE_S,
                                theta = real_theta, freq = low_freq,
                                inf_period = 1/gamma, compliance = low_compliance, q = q0,
                                psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  df$numtests_ideal_qhigh <- sapply(phi_vec, compute_num_tests, 
                                    this_VE_I, this_VE_S,
                                    theta = ideal_theta, freq = high_freq,
                                    inf_period = 1/gamma, compliance = high_compliance, q = qhigh,
                                    psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$numtests_mod_qhigh <- sapply(phi_vec, compute_num_tests,
                                  this_VE_I, this_VE_S,
                                  theta = mod_theta, freq = low_freq,
                                  inf_period = 1/gamma, compliance = high_compliance, q = qhigh,
                                  psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$numtests_real_qhigh <- sapply(phi_vec, compute_num_tests,
                                   this_VE_I, this_VE_S,
                                   theta = real_theta, freq = low_freq,
                                   inf_period = 1/gamma, compliance = low_compliance, q = qhigh,
                                   psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  #reduction per 100 tests
  df$reducpertest_ideal_q0 <- df$cases_averted_ideal_q0 / df$numtests_ideal_q0*100
  df$reducpertest_mod_q0 <- df$cases_averted_mod_q0 / df$numtests_mod_q0*100
  df$reducpertest_real_q0 <- df$cases_averted_real_q0 / df$numtests_real_q0*100
  
  df$reducpertest_ideal_qhigh <- df$cases_averted_ideal_qhigh / df$numtests_ideal_qhigh*100
  df$reducpertest_mod_qhigh <- df$cases_averted_mod_qhigh / df$numtests_mod_qhigh*100
  df$reducpertest_real_qhigh <- df$cases_averted_real_qhigh / df$numtests_real_qhigh*100
  
  # fix when numtests = 0 (currently NA)
  df$reducpertest_ideal_q0[101] <- 0
  df$reducpertest_mod_q0[101] <- 0
  df$reducpertest_real_q0[101] <- 0
  df$reducpertest_ideal_qhigh[101] <- 0
  df$reducpertest_mod_qhigh[101] <- 0
  df$reducpertest_real_qhigh[101] <- 0
  
  E <- ggplot(df, aes(x=phi*100)) +
    geom_line(aes(y = reducpertest_ideal_q0), col = mygreen, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = reducpertest_mod_q0), col = myblue, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = reducpertest_real_q0), col = myyellow, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = 0), col = myred, size = my_linesize) +
    
    geom_line(aes(y = reducpertest_ideal_qhigh), col = mygreen, size = my_linesize) +
    geom_line(aes(y = reducpertest_mod_qhigh), col = myblue, size = my_linesize) +
    geom_line(aes(y = reducpertest_real_qhigh), col = myyellow, size = my_linesize) +
    ylab("") +
    xlab("") +
    #ggtitle("Infections averted\nper 100 tests")+
    #onlyy_theme +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-0.035, 6))
  
  #*
  #* Panel F - Reff, q = 0, 0.8 ####
  #* 
  df$Reff_notesting_q0 <- sapply(phi_vec, compute_Reff, 
                                 this_VE_I, this_VE_S,
                                 theta = 0, q = q0,
                                 psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_idealtesting_q0 <- sapply(phi_vec, compute_Reff, 
                                    this_VE_I, this_VE_S,
                                    theta = ideal_theta, q = q0,
                                    psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_modtesting_q0 <- sapply(phi_vec, compute_Reff, 
                                  this_VE_I, this_VE_S,
                                  theta = mod_theta, q = q0,
                                  psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_realtesting_q0 <- sapply(phi_vec, compute_Reff, 
                                   this_VE_I, this_VE_S,
                                   theta = real_theta, q = q0,
                                   psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  df$Reff_notesting_qhigh <- sapply(phi_vec, compute_Reff, 
                                    this_VE_I, this_VE_S,
                                    theta = 0, q = qhigh,
                                    psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_idealtesting_qhigh <- sapply(phi_vec, compute_Reff, 
                                       this_VE_I, this_VE_S,
                                       theta = ideal_theta, q = qhigh,
                                       psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_modtesting_qhigh <- sapply(phi_vec, compute_Reff, 
                                     this_VE_I, this_VE_S,
                                     theta = mod_theta, q = qhigh,
                                     psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_realtesting_qhigh <- sapply(phi_vec, compute_Reff, 
                                      this_VE_I, this_VE_S,
                                      theta = real_theta, q = qhigh,
                                      psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  F <- ggplot(df, aes(x=phi*100)) +
    geom_hline(yintercept = 1, size = 0.9, alpha = 0.3) +
    geom_line(aes(y = Reff_notesting_q0), col = myred, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = Reff_modtesting_q0), col = myblue,  size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = Reff_idealtesting_q0), col = mygreen,  size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = Reff_realtesting_q0), col = myyellow,  size = 0.8, alpha = 0.6, linetype = "dashed") +
    
    geom_line(aes(y = Reff_notesting_qhigh), col = myred, size = my_linesize) +
    geom_line(aes(y = Reff_modtesting_qhigh), col = myblue, size = my_linesize) +
    geom_line(aes(y = Reff_idealtesting_qhigh), col = mygreen, size = my_linesize) +
    geom_line(aes(y = Reff_realtesting_qhigh), col = myyellow, size = my_linesize) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 6)) + 
    ylab("") + # R_eff 
    #ggtitle(expression(R[eff])) +
    #onlyy_theme +
    xlab("") 
  
  if (this_panel == 1){
    A <- A + onlyy_theme + 
      ggtitle("New daily\ninfections (#)")
    B <- B + onlyy_theme + 
      ggtitle("Transmission mode\n(%)")
    C <- C + nolabels_theme + 
      ggtitle("Infections averted\nrel. to no testing\n(% of pop.)")
    D <- D + nolabels_theme + 
      ggtitle("Composition of\ninfections averted\n(% unvacc.)")
    E <- E + onlyy_theme + 
      ggtitle("Infections averted\nper 100 tests")
    F <- F + nolabels_theme + 
      ggtitle(expression(R[eff]))
    
    panel1sup <- ggarrange(A, NULL, B, NULL, C, NULL, D, NULL, E, NULL, F, NULL,
                           nrow = 1,
                           widths = c(1, -0.05, 1, -0.15, 1, -0.15, 1, -0.1, 1, -0.15, 1, 0.1), 
                           align = "hv",
                           labels = c('a', NA, ' b', NA, '    c', NA, '    d', NA, '  e', NA, '    f', NA),
                           label.y = 0.77)
    
  } else if (this_panel == 2) {
    A <- A + onlyy_theme
    B <- B + onlyy_theme
    C <- C + nolabels_theme
    D <- D + nolabels_theme
    E <- E + onlyy_theme
    F <- F + nolabels_theme
    panel2sup <- ggarrange(A, NULL, B, NULL, C, NULL, D, NULL, E, NULL, F, NULL,
                           nrow = 1,
                           widths = c(1, -0.05, 1, -0.15, 1, -0.15, 1, -0.1, 1, -0.15, 1, 0.1), 
                           align = "hv",
                           labels = c('g', NA, ' h', NA, '    i', NA, '    j', NA, '  k', NA, '    l', NA))
  } else {
    A <- A + xlab("Time (days)")
    C <- C + onlyx_theme 
    D <- D + onlyx_theme
    F <- F + onlyx_theme
    panel3sup <- ggarrange(A, NULL, B, NULL, C, NULL, D, NULL, E, NULL, F, NULL,
                           nrow = 1,
                           widths = c(1, -0.05, 1, -0.15, 1, -0.15, 1, -0.1, 1, -0.15, 1, 0.1), 
                           align = "hv",
                           labels = c('m', NA, ' n', NA, '    o', NA, '    p', NA, '  q', NA, '    r', NA)
    )
  }
}

# supplementary figure
suppfig3 <- ggarrange(panel1sup, panel2sup, panel3sup, nrow = 3,
          heights = c(1.3, 1, 1.1),
          align = "hv")

annotate_figure(suppfig3,
                bottom = text_grob("Population vaccination rate (%)", size = 14, family = "Arial",
                                   vjust = -1.2, hjust = 0))

ggsave("suppfig3.pdf", device = cairo_pdf, width = 14, height = 8)
