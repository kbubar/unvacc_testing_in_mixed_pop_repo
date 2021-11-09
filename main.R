# Main.R - runs SEIR model and generates main text figures and table
# Written by: Kate Bubar and Casey Middleton

# setup.R works correctly if you've already installed fonts. if not, go to setup.R line 13.
source("setup.R") 

# _____________________________________________________________________
# FIG1 - infections with no testing ####
# _____________________________________________________________________
# for loop to run fig 1
for (i in 1){
  #* Panel B - infected over time ####
  this_theta <- 0 # no testing
  this_q <- 0 # well-mixed
  
  df_B <- run_leaky_model(this_phi, this_VE_I, this_VE_S, this_theta, this_q, 
                          psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  B <- ggplot(df_B, aes(x = time)) + 
    geom_line(aes(y = I_v), col = mylightgray, size = my_linesize) + 
    geom_line(aes(y = I_u + I_x), col = mygray, size = my_linesize, linetype = "longdash")  + 
    geom_line(aes(y = I_v + I_u + I_x), col = myblack, size = my_linesize) +
    ylab("Infected (#)") + 
    xlab("Time (days)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 270)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 875)) +
    onlyy_theme 
  
  #*  Panel D - transmission mode over time (i.e. who caused new daily cases) ####
  df <- data.frame(time = t)
  
  list_who_caused <- compute_who_caused_daily_infections(this_phi, this_VE_I, this_VE_S, 
                                                         this_theta, this_q,
                                                         this_psi, this_X_I, this_X_S)
  
  df$cases_in_v_by_v <- unlist(list_who_caused[[1]])
  df$cases_in_v_by_u <- unlist(list_who_caused[[2]])
  df$cases_in_u_by_v <- unlist(list_who_caused[[3]])
  df$cases_in_u_by_u <- unlist(list_who_caused[[4]])
  
  D <- ggplot(df, aes(x = time)) + 
    geom_line(aes(y = cases_in_u_by_v), col = mylightteal, size = my_linesize) + 
    geom_line(aes(y = cases_in_v_by_u), col = mylightorange, size = my_linesize) +
    geom_line(aes(y = cases_in_u_by_u), col = mydarkorange, size = my_linesize) +
    geom_line(aes(y = cases_in_v_by_v), col = mydarkteal, size = my_linesize) + 
    ylab("New daily infections (#) ") + 
    xlab("Time (days)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 270)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
    alllabels_theme
  
  #* Panel C - total infections and breakthrough cases over phi ####
  df <- data.frame(phi = phi_vec)
  df$tot_infections <- sapply(phi_vec, compute_tot_infections, 
                              VE_I = this_VE_I, VE_S = this_VE_S, 
                              theta = this_theta, q = this_q,
                              psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  df$Reff <- sapply(phi_vec, compute_Reff,
                    VE_I = this_VE_I, VE_S = this_VE_S, 
                    theta = this_theta, q = this_q,
                    psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  df$breakthrough <- sapply(phi_vec, compute_percent_breakthrough_infections, 
                            VE_I = this_VE_I, VE_S = this_VE_S, 
                            theta = this_theta, q = this_q,
                            psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  C <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y=breakthrough), col = mylightgray, size = my_linesize) +
    geom_line(aes(y=tot_infections/N*100), col = myblack, size = my_linesize) + 
    ylab("Total infected (%)") + 
    xlab("Population vaccination rate (%)") + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100),
                       sec.axis = sec_axis(~., name="Breakthrough infections (%)")) +
    scale_x_continuous(expand = c(0, 0)) + 
    theme(axis.title.y.right = element_text(color = mylightgray),
          axis.title.y.left = element_text(color = myblack),
          axis.title.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank(),
          plot.margin=unit(c(5.5, 2, 5.5, 2), "pt")) # top, right, bottom, left
  
  Reff_1 <- min(which(df$Reff <= 1)) - 1 
  
  if (Reff_1 <= 100){
    C <- C + geom_vline(xintercept = Reff_1, alpha = 0.5, linetype = "dashed", size = 0.5) 
  }
  
  #* Panel E - cumulative transmission mode over phi ####
  df <- data.frame(phi = phi_vec)
  
  lists_who_caused <- lapply(phi_vec, compute_who_caused_cases_tot, 
                             VE_I = this_VE_I, VE_S = this_VE_S, 
                             theta = this_theta, q = this_q,
                             psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  mat_who_caused <- matrix(unlist(lists_who_caused), ncol=6, byrow=TRUE)
  
  df$cases_in_v_by_ext <- mat_who_caused[,5]
  df$cases_in_u_by_ext <- mat_who_caused[,6]
  df$cases_in_v_by_v <- mat_who_caused[,1]
  df$cases_in_u_by_v <- mat_who_caused[,3]
  df$cases_in_v_by_u <- mat_who_caused[,2]
  df$cases_in_u_by_u <- mat_who_caused[,4]
  
  df_toplot <- melt(df, id = c("phi"))
  
  E <- ggplot(df_toplot, aes(x=phi*100, y=value*100, color = variable)) + 
    geom_line(size = my_linesize) +
    ylab(paste("Transmission mode (%) ")) +
    xlab(expression("Population vaccination rate (%)")) +
    labs(fill = "") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100.1)) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_color_manual(values = c(mygray, mylightgray, mydarkteal, mylightteal, mylightorange,
                                  mydarkorange)) +
    #labels = c("v to v", "v to u", "u to v", "u to u")) + 
    theme(legend.position = "none")
  
  if (Reff_1 <= 100){
    E <- E + geom_vline(xintercept = Reff_1, alpha = 0.5, linetype = "dashed", size = 0.5)
  }
}

# export as cairo_pdf,8x5.5in  
ggarrange(B, NULL, C, NULL, NULL, NULL, D, NULL, E,
          labels = c("b", NA, "c", NA, NA, NA, "d", NA, "e"),
          nrow = 3,
          ncol = 3,
          align = "hv",
          widths = c(1, -0.1, 1),
          heights = c(1, -0.13, 1))

ggsave("fig1_test.pdf", device = cairo_pdf, width = 8, height = 5.5)

# _____________________________________________________________________
# TABLE1 - when transmission is no longer dominated by unvaccinated ####
# i.e. when the cumulative orange curves drop below 50% on fig 1E
# _____________________________________________________________________

mytable <- data.frame(q = c(rep(q0, 12), rep(qhigh, 12)),
                      theta = c(rep(c(rep(0, 3), rep(real_theta, 3), rep(mod_theta, 3), rep(ideal_theta, 3)), 2)),
                      R0 = rep(c(2,4,6), 8),
                      phi = rep(NA, 24))

for (i in 1:24){
  R0 <- mytable[i,]$R0
  alpha <- R0*gamma/N # transmissibility
  
  this_theta <- mytable[i,]$theta
  this_q <- mytable[i,]$q
  
  df <- data.frame(phi = phi_vec)
  
  lists_who_caused <- lapply(phi_vec, compute_who_caused_cases_tot, 
                             VE_I = this_VE_I, VE_S = this_VE_S, 
                             theta = this_theta, q = this_q, 
                             psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  mat_who_caused <- matrix(unlist(lists_who_caused), ncol=6, byrow=TRUE)
  
  df$cases_in_v_by_u <- mat_who_caused[,2]
  df$cases_in_u_by_u <- mat_who_caused[,4]
  
  # get the phi value when cumulative infections by u drop below 50%
  mytable[i,4] <- df[min(which(df$cases_in_u_by_u + df$cases_in_v_by_u < 0.5)), 1]
}

# _____________________________________________________________________
# FIG2 - w/testing ####
# _____________________________________________________________________
this_q <- 0
panels <- c(1,2)

for (this_panel in panels){
  R0 = ifelse(this_panel==1, 4, 6)
  alpha <- R0*gamma/N # transmissibility
  
  R0_ideal_theta <- alpha*N*(1-ideal_theta)/gamma 
  R0_mod_theta <- alpha*N*(1-mod_theta)/gamma 
  R0_real_theta <- alpha*N*(1-real_theta)/gamma 
  
  df <- data.frame(phi = phi_vec)
  # Total infections with various testing scenarios
  df$tot_infections_notesting <- sapply(phi_vec, compute_tot_infections, 
                                        VE_I = this_VE_I, VE_S = this_VE_S, theta = 0, q = this_q,
                                        psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$tot_infections_idealtesting <- sapply(phi_vec, compute_tot_infections, 
                                           VE_I = this_VE_I, VE_S = this_VE_S, theta = ideal_theta, q = this_q,
                                           psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$tot_infections_modtesting <- sapply(phi_vec, compute_tot_infections, 
                                         VE_I = this_VE_I, VE_S = this_VE_S, theta = mod_theta, q = this_q,
                                         psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$tot_infections_realtesting <- sapply(phi_vec, compute_tot_infections, 
                                          VE_I = this_VE_I, VE_S = this_VE_S, theta = real_theta, q = this_q,
                                          psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  # Total number of cases averted 
  df$cases_averted_ideal <- (df$tot_infections_notesting - df$tot_infections_idealtesting)
  df$cases_averted_mod <- (df$tot_infections_notesting - df$tot_infections_modtesting)
  df$cases_averted_real <- (df$tot_infections_notesting - df$tot_infections_realtesting)
  
  # Proportion of infections averted in unvax class
  df$u_infections_notesting <- sapply(phi_vec, compute_u_infections, 
                                      VE_I = this_VE_I, VE_S = this_VE_S, theta = 0, q = this_q,
                                      psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$u_infections_idealtesting <- sapply(phi_vec, compute_u_infections, 
                                         VE_I = this_VE_I, VE_S = this_VE_S, theta = ideal_theta, q = this_q,
                                         psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$u_infections_modtesting <- sapply(phi_vec, compute_u_infections, 
                                       VE_I = this_VE_I, VE_S = this_VE_S, theta = mod_theta, q = this_q,
                                       psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$u_infections_realtesting <- sapply(phi_vec, compute_u_infections, 
                                        VE_I = this_VE_I, VE_S = this_VE_S, theta = real_theta, q = this_q,
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
                              VE_I = this_VE_I, VE_S = this_VE_S, theta = 0, q = this_q, 
                              psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_idealtesting <- sapply(phi_vec, compute_Reff, 
                                 VE_I = this_VE_I, VE_S = this_VE_S, theta = ideal_theta, q = this_q,
                                 psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_modtesting <- sapply(phi_vec, compute_Reff, 
                               VE_I = this_VE_I, VE_S = this_VE_S, theta = mod_theta, q = this_q,
                               psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_realtesting <- sapply(phi_vec, compute_Reff, 
                                VE_I = this_VE_I, VE_S = this_VE_S, theta = real_theta, q = this_q,
                                psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
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
    geom_hline(yintercept = 1, size = 0.5, linetype = "dashed", alpha = 0.5) +
    geom_line(aes(y = Reff_notesting), col = myred, size = my_linesize) +
    geom_line(aes(y = Reff_modtesting), col = myblue, size = my_linesize) +
    geom_line(aes(y = Reff_idealtesting), col = mygreen, size = my_linesize) +
    geom_line(aes(y = Reff_realtesting), col = myyellow, size = my_linesize) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 4)) + 
    ylab("") + # R_eff 
    xlab("") 
  
  if (this_panel == 1){
    A <- A + onlyy_theme + 
      ggtitle("Total infections\n(% of pop.)") + 
      theme(plot.title = element_text(size = 13))
    B <- B + nolabels_theme + 
      ggtitle("Infections averted\n(% of pop.)") + 
      theme(plot.title = element_text(size = 13))
    C <- C + onlyy_theme + 
      ggtitle(expression(R[eff]))+ 
      theme(plot.title = element_text(size = 13))
    
    panel1 <- ggarrange(A, NULL, B, NULL, C, NULL,
                        nrow = 1,
                        widths = c(1, -0.2, 1, -0.15, 1, -0.1), 
                        align = "hv",
                        labels = c(' a', NA, '     b', NA, '    c', NA),
                        label.y = 0.82)
    
  } else {
    B <- B + onlyx_theme
    panel2 <- ggarrange(A, NULL, B, NULL, C, NULL,
                        nrow = 1,
                        widths = c(1, -0.2, 1, -0.15, 1, -0.1), 
                        align = "hv",
                        labels = c(' d', NA, '     e', NA, '    f', NA))
  }
}

# export 2000x800
fig2 <- ggarrange(panel1, panel2, nrow = 2,
                  heights = c(1.13, 1),
                  align = "hv")

annotate_figure(fig2,
                bottom = text_grob("Population vaccination rate (%)", size = 14, family = "Arial",
                                   vjust = -1.2))

ggsave("fig2.pdf", device = cairo_pdf, width = 8, height = 5)

# _____________________________________________________________________
# FIG3 - w/homophily ####
# _____________________________________________________________________
this_theta <- 0
R0 <- 4
alpha <- R0*gamma/N # transmissibility

for (i in 1){
  #*
  #* Panel A - transmission modes over time, phi = 55%, q = 0, 0.8 ####
  #* 
  df <- data.frame(time = t)
  
  list_who_caused <- compute_who_caused_daily_infections(this_phi, this_VE_I, this_VE_S, 
                                                         this_theta, q = q0,
                                                         this_psi, this_X_I, this_X_S)
  df$cases_in_v_by_v_q0 <- unlist(list_who_caused[[1]])
  df$cases_in_v_by_u_q0 <- unlist(list_who_caused[[2]])
  df$cases_in_u_by_v_q0 <- unlist(list_who_caused[[3]])
  df$cases_in_u_by_u_q0 <- unlist(list_who_caused[[4]])
  
  list_who_caused <- compute_who_caused_daily_infections(this_phi, this_VE_I, this_VE_S, 
                                                         this_theta, q = qhigh,
                                                         this_psi, this_X_I, this_X_S)
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
    ggtitle("New daily\ninfections (#)") +
    xlab("Time (days)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 150), breaks = c(0, 75, 150)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
    alllabels_theme
  
  #*
  #* Panel B - line plot of transmission modes over phi with q = 0.8 ####
  #* 
  df <- data.frame(phi = phi_vec)
  
  lists_who_caused <- lapply(phi_vec, compute_who_caused_cases_tot, 
                             VE_I = this_VE_I, VE_S = this_VE_S, 
                             theta = this_theta, q = q0,
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
                                   theta = this_theta, q = qhigh,
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
    #onlyy_theme +
    ggtitle("Transmission mode\n(%)") +
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
    onlyx_theme + 
    ggtitle("Infections averted\nrel. to not testing\n(% of pop.)") +
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
  
  # fix anywhere that had NaN because 0 in the denominator
  this_elem <- which(is.na(df[101,])) 
  df[101,this_elem] <- 0
  
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
    onlyx_theme +
    ggtitle("Composition of\ninfections averted\n(% unvacc.)")+
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(-1, 100)) 
  
  #*
  #* Panel E - Reff, q = 0, 0.8 ####
  #* 
  df$Reff_notesting_q0 <- sapply(phi_vec, compute_Reff, 
                                 VE_I = this_VE_I, VE_S = this_VE_S, 
                                 theta = 0, q = q0,
                                 psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_idealtesting_q0 <- sapply(phi_vec, compute_Reff, 
                                    VE_I = this_VE_I, VE_S = this_VE_S,  
                                    theta = ideal_theta, q = q0,
                                    psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_modtesting_q0 <- sapply(phi_vec, compute_Reff, 
                                  VE_I = this_VE_I, VE_S = this_VE_S, 
                                  theta = mod_theta, q = q0,
                                  psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_realtesting_q0 <- sapply(phi_vec, compute_Reff, 
                                   VE_I = this_VE_I, VE_S = this_VE_S,  
                                   theta = real_theta, q = q0,
                                   psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  df$Reff_notesting_qhigh <- sapply(phi_vec, compute_Reff, 
                                    VE_I = this_VE_I, VE_S = this_VE_S,
                                    theta = 0, q = qhigh,
                                    psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_idealtesting_qhigh <- sapply(phi_vec, compute_Reff, 
                                       VE_I = this_VE_I, VE_S = this_VE_S, 
                                       theta = ideal_theta, q = qhigh,
                                       psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_modtesting_qhigh <- sapply(phi_vec, compute_Reff, 
                                     VE_I = this_VE_I, VE_S = this_VE_S, 
                                     theta = mod_theta, q = qhigh,
                                     psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  df$Reff_realtesting_qhigh <- sapply(phi_vec, compute_Reff, 
                                      VE_I = this_VE_I, VE_S = this_VE_S, 
                                      theta = real_theta, q = qhigh,
                                      psi = this_psi, X_I = this_X_I, X_S = this_X_S)
  
  E <- ggplot(df, aes(x=phi*100)) +
    geom_hline(yintercept = 1, size = 0.5, alpha = 0.5, linetype = "dashed") +
    geom_line(aes(y = Reff_notesting_q0), col = myred, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = Reff_modtesting_q0), col = myblue,  size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = Reff_idealtesting_q0), col = mygreen,  size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = Reff_realtesting_q0), col = myyellow,  size = 0.8, alpha = 0.6, linetype = "dashed") +
    
    geom_line(aes(y = Reff_notesting_qhigh), col = myred, size = my_linesize) +
    geom_line(aes(y = Reff_modtesting_qhigh), col = myblue, size = my_linesize) +
    geom_line(aes(y = Reff_idealtesting_qhigh), col = mygreen, size = my_linesize) +
    geom_line(aes(y = Reff_realtesting_qhigh), col = myyellow, size = my_linesize) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 4)) + 
    ylab("") + # R_eff 
    xlab("") +
    ggtitle(expression(R[eff]))
}

fig3 <- ggarrange(A, NULL, B, NULL, C, NULL, D, NULL, E, NULL,
                    nrow = 1,
                    widths = c(1, -0.09, 1, -0.2, 1, -0.2, 1 , -0.09, 1, 0.1),
                    align = "hv",
                    labels = c(' a', NA, ' b', NA,'     c',NA, '     d', NA,'   e', NA),
                    label.y = 0.79)

annotate_figure(fig3,
                bottom = text_grob("Population vaccination rate (%)", size = 14, family = "Arial", 
                                   vjust = -1.2, hjust = 0))

ggsave("fig3.pdf", device = cairo_pdf, width = 10, height = 3.5)
