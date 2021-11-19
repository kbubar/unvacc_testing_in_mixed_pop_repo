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
                          psi = this_psi, X_I = this_X_I, X_S = this_X_S, 
                          H_I = this_H_I, H_S = this_H_S)
  
  B <- ggplot(df_B, aes(x = time)) + 
    geom_line(aes(y = I_v + I_h), col = mylightgray, size = my_linesize) + 
    geom_line(aes(y = I_u + I_x), col = mygray, size = my_linesize, linetype = "longdash")  + 
    geom_line(aes(y = I_v + I_u + I_x + I_h), col = myblack, size = my_linesize) +
    ylab("Infected (#)") + 
    xlab("Time (days)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 200)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) + # 2500 for R0 = 6
    onlyy_theme 
  
  #*  Panel D - transmission mode over time (i.e. who caused new daily cases) ####
  df <- data.frame(time = t)
  
  list_who_caused <- compute_who_caused_daily_infections(this_phi, this_VE_I, this_VE_S, 
                                                         this_theta, this_q,
                                                         this_psi, this_X_I, this_X_S,
                                                         this_H_I, this_H_S)
  
  df$cases_in_v_by_v <- unlist(list_who_caused[[1]])
  df$cases_in_v_by_u <- unlist(list_who_caused[[2]])
  df$cases_in_u_by_v <- unlist(list_who_caused[[3]])
  df$cases_in_u_by_u <- unlist(list_who_caused[[4]])
  df$cases_in_v_by_ext <- unlist(list_who_caused[[5]])
  df$cases_in_u_by_ext <- unlist(list_who_caused[[6]])
  
  D <- ggplot(df, aes(x = time)) + 
    geom_line(aes(y = cases_in_v_by_ext), col = mygray, size = my_linesize) +
    geom_line(aes(y = cases_in_u_by_ext), col = mylightgray, size = my_linesize) + 
    geom_line(aes(y = cases_in_u_by_v), col = mylightteal, size = my_linesize) + 
    geom_line(aes(y = cases_in_v_by_u), col = mylightorange, size = my_linesize) +
    geom_line(aes(y = cases_in_u_by_u), col = mydarkorange, size = my_linesize) +
    geom_line(aes(y = cases_in_v_by_v), col = mydarkteal, size = my_linesize) + 
    ylab("New daily infections (#) ") + 
    xlab("Time (days)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 200)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 80)) + # C(0, 200) for R0 = 6
    alllabels_theme
  
  #* Panel C - total infections and breakthrough cases over phi ####
  df <- data.frame(phi = phi_vec)
  df$tot_infections <- sapply(phi_vec, compute_tot_infections, 
                              VE_I = this_VE_I, VE_S = this_VE_S, 
                              theta = this_theta, q = this_q,
                              psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                              H_I = this_H_I, H_S = this_H_S)
  
  df$Reff <- sapply(phi_vec, compute_Reff,
                    VE_I = this_VE_I, VE_S = this_VE_S, 
                    theta = this_theta, q = this_q,
                    psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                    H_I = this_H_I, H_S = this_H_S)
  
  df$breakthrough <- sapply(phi_vec, compute_percent_breakthrough_infections, 
                            VE_I = this_VE_I, VE_S = this_VE_S, 
                            theta = this_theta, q = this_q,
                            psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                            H_I = this_H_I, H_S = this_H_S)
  
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
  
  Reff_1 <- min(which(df$Reff <= 1),500) - 1 
  if (Reff_1 <= 100){
    C <- C + geom_vline(xintercept = Reff_1, alpha = 0.5, linetype = "dashed", size = 0.5) 
  }
  inf_transition <- min(which(df$breakthrough >= 50)) - 1
  C <- C + geom_vline(xintercept = inf_transition, alpha = 0.5, linetype = "dashed", size = 0.5, col = mypurple) 
  
  #* Panel E - cumulative transmission mode over phi ####
  df <- data.frame(phi = phi_vec)
  
  lists_who_caused <- lapply(phi_vec, compute_who_caused_cases_tot, 
                             VE_I = this_VE_I, VE_S = this_VE_S, 
                             theta = this_theta, q = this_q,
                             psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                             H_I = this_H_I, H_S = this_H_S)
  
  mat_who_caused <- matrix(unlist(lists_who_caused), ncol=6, byrow=TRUE)
  
  df$cases_in_v_by_ext <- mat_who_caused[,5]
  df$cases_in_u_by_ext <- mat_who_caused[,6]
  df$cases_in_v_by_v <- mat_who_caused[,1]
  df$cases_in_u_by_v <- mat_who_caused[,3]
  df$cases_in_v_by_u <- mat_who_caused[,2]
  df$cases_in_u_by_u <- mat_who_caused[,4]
  
  df_toplot <- melt(df, id = c("phi"))
  
  df$total_by_v <- df$cases_in_u_by_v + df$cases_in_v_by_v
  df$total_by_u <- df$cases_in_u_by_u + df$cases_in_v_by_u
 
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
  trans_transition <- min(which(df$total_by_u < df$total_by_v)) - 1
  E <- E + geom_vline(xintercept = trans_transition, alpha = 0.5, linetype = "dashed", size = 0.5, col = mypurple) 
}

# export as cairo_pdf,8x5.5in  
ggarrange(B, NULL, C, NULL, NULL, NULL, D, NULL, E,
          labels = c("b", NA, "c", NA, NA, NA, "d", NA, "e"),
          #labels = c("a", NA, "b", NA, NA, NA, "c", NA, "d"), # for supp R0 = 6
          nrow = 3,
          ncol = 3,
          align = "hv",
          widths = c(1, -0.1, 1),
          heights = c(1, -0.13, 1))

ggsave("fig1.pdf", device = cairo_pdf, width = 8, height = 5.5)

# _____________________________________________________________________
# TABLE1 - when transmission is no longer dominated by unvaccinated ####
# i.e. when the cumulative orange curves drop below 50% on fig 1E
# _____________________________________________________________________

#mytable <- data.frame(q = c(rep(q0, 12), rep(qhigh, 12)),
#                      theta = c(rep(c(rep(0, 3), rep(theta_50, 3), rep(theta_99, 3), rep(ideal_theta, 3)), 2)),
#                      R0 = rep(c(2,4,6), 8),
#                      phi = rep(NA, 24))
# for (i in 1:24){
#   R0 <- mytable[i,]$R0
#   alpha <- R0*gamma/N # transmissibility
#   
#   this_theta <- mytable[i,]$theta
#   this_q <- mytable[i,]$q
#   
#   df <- data.frame(phi = phi_vec)
#   
#   lists_who_caused <- lapply(phi_vec, compute_who_caused_cases_tot, 
#                              VE_I = this_VE_I, VE_S = this_VE_S, 
#                              theta = this_theta, q = this_q, 
#                              psi = this_psi, X_I = this_X_I, X_S = this_X_S,
#                              H_I = this_H_I, H_S = this_H_S)
#   mat_who_caused <- matrix(unlist(lists_who_caused), ncol=6, byrow=TRUE)
#   
#   df$cases_in_v_by_v <- mat_who_caused[,1]
#   df$cases_in_u_by_v <- mat_who_caused[,3]
#   df$cases_in_v_by_u <- mat_who_caused[,2]
#   df$cases_in_u_by_u <- mat_who_caused[,4]
#   
#   tot <- df$cases_in_v_by_v + df$cases_in_u_by_v + df$cases_in_v_by_u + df$cases_in_u_by_u
#   # get the phi value when cumulative infections by u drop below 50%
#   mytable[i,4] <- df[min(which((df$cases_in_u_by_u + df$cases_in_v_by_u)/tot < 0.5)), 1]
# }

my_list <- lapply(phi_vec, compute_dominant_transmission,
                  VE_I = this_VE_I, VE_S = this_VE_S, 
                  theta = theta_99, q = 0,
                  psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                  H_I = this_H_I, H_S = this_H_S)
my_mat <- matrix(unlist(my_list))

print(min(which(my_mat < 0.5)) - 1)

# who makes up who is infected (R0 = 4)
# theta_99 = 75
# theta_50 = 73

# who makes up who is infected (R0 = 6)
# theta_99 = 67
# theta_50 = 63

## 
# who dominates transmission (R0 = 4) // without external
# theta_99 = 65 // 67
# theta_50 = 72 // 73

# who dominates transmission (R0 = 6) // without external
# theta_99 = 60 // 61
# theta_50 = 67 // 67


# _____________________________________________________________________
# FIG3 - w/testing ####
# _____________________________________________________________________
this_q <- 0
panels <- c(1,2)

for (this_panel in panels){
  R0 = ifelse(this_panel==1, 4, 6)
  alpha <- R0*gamma/N # transmissibility
  
  R0_theta_99 <- alpha*N*(1-theta_99)/gamma 
  R0_theta_50 <- alpha*N*(1-theta_50)/gamma 
  
  df <- data.frame(phi = phi_vec)
  # Total infections with various testing scenarios
  df$tot_infections_notesting <- sapply(phi_vec, compute_tot_infections, 
                                        VE_I = this_VE_I, VE_S = this_VE_S, theta = 0, q = this_q,
                                        psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                        H_I = this_H_I, H_S = this_H_S)
  df$tot_infections_99testing <- sapply(phi_vec, compute_tot_infections, 
                                         VE_I = this_VE_I, VE_S = this_VE_S, theta = theta_99, q = this_q,
                                         psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                         H_I = this_H_I, H_S = this_H_S)
  df$tot_infections_50testing <- sapply(phi_vec, compute_tot_infections, 
                                          VE_I = this_VE_I, VE_S = this_VE_S, theta = theta_50, q = this_q,
                                          psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                          H_I = this_H_I, H_S = this_H_S)
  
  # Total number of cases averted 
  df$cases_averted_99 <- (df$tot_infections_notesting - df$tot_infections_99testing)
  df$cases_averted_50 <- (df$tot_infections_notesting - df$tot_infections_50testing)
  
  # Proportion of infections averted in unvax class
  df$u_infections_notesting <- sapply(phi_vec, compute_u_infections, 
                                      VE_I = this_VE_I, VE_S = this_VE_S, theta = 0, q = this_q,
                                      psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                      H_I = this_H_I, H_S = this_H_S)
  df$u_infections_99testing <- sapply(phi_vec, compute_u_infections, 
                                       VE_I = this_VE_I, VE_S = this_VE_S, theta = theta_99, q = this_q,
                                       psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                       H_I = this_H_I, H_S = this_H_S)
  df$u_infections_50testing <- sapply(phi_vec, compute_u_infections, 
                                        VE_I = this_VE_I, VE_S = this_VE_S, theta = theta_50, q = this_q,
                                        psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                        H_I = this_H_I, H_S = this_H_S)
  
  df$prop_cases_averted_u_99 <- (df$u_infections_notesting - df$u_infections_99testing) / df$cases_averted_99 * 100
  df$prop_cases_averted_u_50 <- (df$u_infections_notesting - df$u_infections_50testing) / df$cases_averted_50 * 100
  
  df$num_cases_averted_u_99 <- (df$u_infections_notesting - df$u_infections_99testing) 
  df$num_cases_averted_u_50 <- (df$u_infections_notesting - df$u_infections_50testing) 
  df$num_cases_averted_v_99 <- (df$cases_averted_99 - df$num_cases_averted_u_99) 
  df$num_cases_averted_v_50 <- (df$cases_averted_50 - df$num_cases_averted_u_50) 
  
  # Reff
  df$Reff_notesting <- sapply(phi_vec, compute_Reff, 
                              VE_I = this_VE_I, VE_S = this_VE_S, theta = 0, q = this_q, 
                              psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                              H_I = this_H_I, H_S = this_H_S)
  df$Reff_99testing <- sapply(phi_vec, compute_Reff, 
                               VE_I = this_VE_I, VE_S = this_VE_S, theta = theta_99, q = this_q,
                               psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                               H_I = this_H_I, H_S = this_H_S)
  df$Reff_50testing <- sapply(phi_vec, compute_Reff, 
                                VE_I = this_VE_I, VE_S = this_VE_S, theta = theta_50, q = this_q,
                                psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                H_I = this_H_I, H_S = this_H_S)
  
  Reff_1 <- min(which(df$Reff_notesting <= 1), 500) - 1 
  
  # Reduction per test 
  df$numtests_99testing <- sapply(phi_vec, compute_num_tests, 
                                   VE_I = this_VE_I, VE_S = this_VE_S, 
                                   theta = theta_99, freq = low_freq, 
                                   inf_period = 1/gamma, compliance = high_compliance, q = this_q,
                                   psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                   H_I = this_H_I, H_S = this_H_S)
  df$numtests_50testing <- sapply(phi_vec, compute_num_tests, 
                                    VE_I = this_VE_I, VE_S = this_VE_S, 
                                    theta = theta_50, freq = low_freq, 
                                    inf_period = 1/gamma, compliance = low_compliance, q = this_q,
                                    psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                    H_I = this_H_I, H_S = this_H_S)

  df$reducpertest_99testing <- df$cases_averted_99 / df$numtests_99testing
  df$reducpertest_50testing <- df$cases_averted_50 / df$numtests_50testing
  df$reducpertest_notesting <- 0
  
  # fix anywhere that had NaN because 0 in the denominator
  this_elem <- which(is.na(df[101,])) 
  df[101,this_elem] <- 0
  
  A <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y = tot_infections_notesting/1000), col = mylightgray, size = my_linesize) +  
    geom_line(aes(y = tot_infections_99testing/1000), col = myblue, size = my_linesize) + 
    geom_line(aes(y = tot_infections_50testing/1000), col = myyellow, size = my_linesize) + 
    ylab("") + # Total infections (%)
    xlab("") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, N/1000)) 
  if (Reff_1 <= 100){
    A <- A + geom_vline(xintercept = Reff_1, alpha = 0.5, linetype = "dashed", size = 0.5, col = mygray) 
  }
  
  B <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y=cases_averted_99/1000), col = myblue, size = my_linesize) +
    geom_line(aes(y = cases_averted_50/1000), col = myyellow, size = my_linesize) + 
    geom_line(aes(y = 0), col = mylightgray, size = my_linesize) +  
    ylab("") + # Cases averted (thousands)
    xlab("") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-0.1, 10))  
  if (Reff_1 <= 100){
    B <- B + geom_vline(xintercept = Reff_1, alpha = 0.5, linetype = "dashed", size = 0.5, col = mygray) 
  }
  
  C <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y=reducpertest_99testing * 100), col = myblue, size = my_linesize) +
    geom_line(aes(y = reducpertest_50testing * 100), col = myyellow, size = my_linesize) +
    geom_line(aes(y = 0), col = mylightgray, size = my_linesize) +
    ylab("") + # Cases averted per 100 tests (#)
    xlab("") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-0.05, 7))
  if (Reff_1 <= 100){
    C <- C + geom_vline(xintercept = Reff_1, alpha = 0.5, linetype = "dashed", size = 0.5, col = mygray) 
  }
  # Reff_99 <- min(which(df$Reff_99testing <= 1), 500) - 1 
  # Reff_50 <- min(which(df$Reff_50testing <= 1), 500) - 1 
  # if (Reff_1 <= 100){
  #   C <- C + 
  #     geom_vline(xintercept = Reff_50, alpha = 0.7, linetype = "dashed", size = 0.5, col = myyellow) +
  #     geom_vline(xintercept = Reff_99, alpha = 0.7, linetype = "dashed", size = 0.5, col = myblue) 
  # }
  
  D <- ggplot(df, aes(x=phi*100)) +
    geom_hline(yintercept = 1, size = 0.5, linetype = "dashed", alpha = 0.5, col = mygray) +
    geom_line(aes(y = Reff_notesting), col = mylightgray, size = my_linesize) +
    geom_line(aes(y = Reff_99testing), col = myblue, size = my_linesize) +
    geom_line(aes(y = Reff_50testing), col = myyellow, size = my_linesize) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 50, 100)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 5)) + 
    ylab("") + # R_eff 
    xlab("") 
  
  if (this_panel == 1){
    A <- A + onlyy_theme + 
      ggtitle("Total infections\n (thousands)") + 
      theme(plot.title = element_text(size = 13))
    B <- B + onlyy_theme + 
      ggtitle("Infections averted\n (thousands)") + 
      theme(plot.title = element_text(size = 13))
    C <- C + onlyy_theme + 
      ggtitle("Infections averted\n per 100 tests")+ 
      theme(plot.title = element_text(size = 13))
    D <- D + onlyy_theme + 
      ggtitle(expression(R[eff]))+ 
      theme(plot.title = element_text(size = 13))
    
    panel1 <- ggarrange(A, NULL, B, NULL, C, NULL, D, NULL,
                        nrow = 1,
                        widths = c(1, -0.05, 1, -0.15, 1, -0.15, 1, 0.1), 
                        align = "hv",
                        labels = c(' a', NA, ' b', NA, '    c', NA, '    d', NA),
                        label.y = 0.82)
    
  } else {
    panel2 <- ggarrange(A, NULL, B, NULL, C, NULL, D, NULL,
                        nrow = 1,
                        widths = c(1, -0.05, 1, -0.15, 1, -0.15, 1, 0.1),
                        align = "hv",
                        labels = c(' e', NA, '  f', NA, '    g', NA, '    h', NA))
  }
}

# export 2000x800
fig3 <- ggarrange(panel1, panel2, nrow = 2,
                  heights = c(1.05, 1),
                  align = "hv")

annotate_figure(fig3,
                bottom = text_grob("Population vaccination rate (%)", size = 14, family = "Arial",
                                   vjust = -1.2))

ggsave("fig3_v1.pdf", device = cairo_pdf, width = 8, height = 5)

# _____________________________________________________________________
# OLDFIG3 - w/homophily ####
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
                                                         this_psi, this_X_I, this_X_S,
                                                         this_H_I, this_H_S)
  df$cases_in_v_by_v_q0 <- unlist(list_who_caused[[1]])
  df$cases_in_v_by_u_q0 <- unlist(list_who_caused[[2]])
  df$cases_in_u_by_v_q0 <- unlist(list_who_caused[[3]])
  df$cases_in_u_by_u_q0 <- unlist(list_who_caused[[4]])
  
  list_who_caused <- compute_who_caused_daily_infections(this_phi, this_VE_I, this_VE_S, 
                                                         this_theta, q = qhigh,
                                                         this_psi, this_X_I, this_X_S,
                                                         this_H_I, this_H_S)
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
                             psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                             H_I = this_H_I, H_S = this_H_S)
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
                                   psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                   H_I = this_H_I, H_S = this_H_S)
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
                                           psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                           H_I = this_H_I, H_S = this_H_S)
  df$tot_infections_99_q0 <- sapply(phi_vec, compute_tot_infections, 
                                     VE_I = this_VE_I, VE_S = this_VE_S, 
                                     theta = theta_99, q = q0,
                                     psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                     H_I = this_H_I, H_S = this_H_S)
  df$tot_infections_50_q0 <- sapply(phi_vec, compute_tot_infections, 
                                      VE_I = this_VE_I, VE_S = this_VE_S, 
                                      theta = theta_50, q = q0,
                                      psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                      H_I = this_H_I, H_S = this_H_S)
  
  df$tot_infections_notesting_qhigh <- sapply(phi_vec, compute_tot_infections, 
                                              VE_I = this_VE_I, VE_S = this_VE_S, 
                                              theta = 0, q = qhigh,
                                              psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                              H_I = this_H_I, H_S = this_H_S)
  df$tot_infections_99_qhigh <- sapply(phi_vec, compute_tot_infections, 
                                        VE_I = this_VE_I, VE_S = this_VE_S, 
                                        theta = theta_99, q = qhigh,
                                        psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                        H_I = this_H_I, H_S = this_H_S)
  df$tot_infections_50_qhigh <- sapply(phi_vec, compute_tot_infections, 
                                         VE_I = this_VE_I, VE_S = this_VE_S, 
                                         theta = theta_50, q = qhigh,
                                         psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                         H_I = this_H_I, H_S = this_H_S)
  
  df$cases_averted_99_q0 <- (df$tot_infections_notesting_q0 - df$tot_infections_99_q0)
  df$cases_averted_50_q0 <- (df$tot_infections_notesting_q0 - df$tot_infections_50_q0)
  
  df$cases_averted_99_qhigh <- (df$tot_infections_notesting_qhigh - df$tot_infections_99_qhigh)
  df$cases_averted_50_qhigh <- (df$tot_infections_notesting_qhigh - df$tot_infections_50_qhigh)
  
  C <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y=cases_averted_99_q0/N*100), col = myblue, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = cases_averted_50_q0/N*100), col = myyellow, size = 0.8, alpha = 0.6, linetype = "dashed") + 
    geom_line(aes(y = 0), col = myred, size = my_linesize) +  
    geom_line(aes(y=cases_averted_99_qhigh/N*100), col = myblue, size = my_linesize) +
    geom_line(aes(y = cases_averted_50_qhigh/N*100), col = myyellow, size = my_linesize) +
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
                                         psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                         H_I = this_H_I, H_S = this_H_S)
  df$u_infections_99testing_q0 <- sapply(phi_vec, compute_u_infections, 
                                          VE_I = this_VE_I, VE_S = this_VE_S, 
                                          theta = theta_99, q = q0,
                                          psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                          H_I = this_H_I, H_S = this_H_S)
  df$u_infections_50testing_q0 <- sapply(phi_vec, compute_u_infections, 
                                           VE_I = this_VE_I, VE_S = this_VE_S, 
                                           theta = theta_50, q = q0,
                                           psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                           H_I = this_H_I, H_S = this_H_S)
  
  df$prop_cases_averted_u_99_q0 <- (df$u_infections_notesting_q0 - df$u_infections_99testing_q0) / df$cases_averted_99_q0 * 100
  df$prop_cases_averted_u_50_q0 <- (df$u_infections_notesting_q0 - df$u_infections_50testing_q0) / df$cases_averted_50_q0 * 100
  
  df$num_cases_averted_u_99_q0 <- (df$u_infections_notesting_q0 - df$u_infections_99testing_q0) 
  df$num_cases_averted_u_50_q0 <- (df$u_infections_notesting_q0 - df$u_infections_50testing_q0) 
  df$num_cases_averted_v_99_q0 <- (df$cases_averted_99_q0 - df$num_cases_averted_u_99_q0) 
  df$num_cases_averted_v_50_q0 <- (df$cases_averted_50_q0 - df$num_cases_averted_u_50_q0)
  
  df$u_infections_notesting_qhigh <- sapply(phi_vec, compute_u_infections, 
                                            VE_I = this_VE_I, VE_S = this_VE_S, 
                                            theta = 0, q = qhigh,
                                            psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                            H_I = this_H_I, H_S = this_H_S)
  df$u_infections_99testing_qhigh <- sapply(phi_vec, compute_u_infections, 
                                             VE_I = this_VE_I, VE_S = this_VE_S, 
                                             theta = theta_99, q = qhigh,
                                             psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                             H_I = this_H_I, H_S = this_H_S)
  df$u_infections_50testing_qhigh <- sapply(phi_vec, compute_u_infections, 
                                              VE_I = this_VE_I, VE_S = this_VE_S, 
                                              theta = theta_50, q = qhigh,
                                              psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                              H_I = this_H_I, H_S = this_H_S)
  
  df$prop_cases_averted_u_99_qhigh <- (df$u_infections_notesting_qhigh - df$u_infections_99testing_qhigh) / df$cases_averted_99_qhigh * 100
  df$prop_cases_averted_u_50_qhigh <- (df$u_infections_notesting_qhigh - df$u_infections_50testing_qhigh) / df$cases_averted_50_qhigh * 100
  
  df$num_cases_averted_u_99_qhigh <- (df$u_infections_notesting_qhigh - df$u_infections_99testing_qhigh) 
  df$num_cases_averted_u_50_qhigh <- (df$u_infections_notesting_qhigh - df$u_infections_50testing_qhigh) 
  df$num_cases_averted_v_99_qhigh <- (df$cases_averted_99_qhigh - df$num_cases_averted_u_99_qhigh) 
  df$num_cases_averted_v_50_qhigh <- (df$cases_averted_50_qhigh - df$num_cases_averted_u_50_qhigh)
  
  # fix anywhere that had NaN because 0 in the denominator
  this_elem <- which(is.na(df[101,])) 
  df[101,this_elem] <- 0
  
  D <- ggplot(df, aes(x=phi*100)) + 
    geom_line(aes(y=prop_cases_averted_u_99_q0), col = myblue, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y=prop_cases_averted_u_50_q0), col = myyellow, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y=prop_cases_averted_u_99_qhigh), col = myblue, size = my_linesize) +
    geom_line(aes(y=prop_cases_averted_u_50_qhigh), col = myyellow, size = my_linesize) +
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
                                 psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                 H_I = this_H_I, H_S = this_H_S)
  df$Reff_99testing_q0 <- sapply(phi_vec, compute_Reff, 
                                  VE_I = this_VE_I, VE_S = this_VE_S, 
                                  theta = theta_99, q = q0,
                                  psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                  H_I = this_H_I, H_S = this_H_S)
  df$Reff_50testing_q0 <- sapply(phi_vec, compute_Reff, 
                                   VE_I = this_VE_I, VE_S = this_VE_S,  
                                   theta = theta_50, q = q0,
                                   psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                   H_I = this_H_I, H_S = this_H_S)
  
  df$Reff_notesting_qhigh <- sapply(phi_vec, compute_Reff, 
                                    VE_I = this_VE_I, VE_S = this_VE_S,
                                    theta = 0, q = qhigh,
                                    psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                    H_I = this_H_I, H_S = this_H_S)
  df$Reff_99testing_qhigh <- sapply(phi_vec, compute_Reff, 
                                     VE_I = this_VE_I, VE_S = this_VE_S, 
                                     theta = theta_99, q = qhigh,
                                     psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                     H_I = this_H_I, H_S = this_H_S)
  df$Reff_50testing_qhigh <- sapply(phi_vec, compute_Reff, 
                                      VE_I = this_VE_I, VE_S = this_VE_S, 
                                      theta = theta_50, q = qhigh,
                                      psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                      H_I = this_H_I, H_S = this_H_S)
  
  E <- ggplot(df, aes(x=phi*100)) +
    geom_hline(yintercept = 1, size = 0.5, alpha = 0.5, linetype = "dashed") +
    geom_line(aes(y = Reff_notesting_q0), col = myred, size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = Reff_99testing_q0), col = myblue,  size = 0.8, alpha = 0.6, linetype = "dashed") +
    geom_line(aes(y = Reff_50testing_q0), col = myyellow,  size = 0.8, alpha = 0.6, linetype = "dashed") +
    
    geom_line(aes(y = Reff_notesting_qhigh), col = myred, size = my_linesize) +
    geom_line(aes(y = Reff_99testing_qhigh), col = myblue, size = my_linesize) +
    geom_line(aes(y = Reff_50testing_qhigh), col = myyellow, size = my_linesize) +
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
