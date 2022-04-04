source("setup.R")

# _____________________________________________________________________
# FIGURE 4 - giving intuition about the impact of testing across vaccination rates #####
# Three time-series plots for regions 1, 2, and 3 with and without testing
# Reff with and without testing over phi
# _____________________________________________________________________

notesting_theta <- 0 # no testing scenario
testing_theta <- theta_99
this_q <- 0 # well-mixed

for (i in 1) {
  df <- data.frame(time = t)
  
  #* Panel A - infected over time for phi = 0.2 ####
  this_phi <- 0.25
  
  df_notesting_reg1 <- run_leaky_model(this_phi, this_VE_I, this_VE_S, notesting_theta, this_q,
                                       psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                       H_I = this_H_I, H_S = this_H_S)
  df$notesting_reg1 <- df_notesting_reg1$I_v + df_notesting_reg1$I_u + df_notesting_reg1$I_x + df_notesting_reg1$I_h
  
  df_testing_reg1 <- run_leaky_model(this_phi, this_VE_I, this_VE_S, testing_theta, this_q,
                                     psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                     H_I = this_H_I, H_S = this_H_S)
  df$testing_reg1 <- df_testing_reg1$I_v + df_testing_reg1$I_u + df_testing_reg1$I_x + df_testing_reg1$I_h
  
  A <- ggplot(df, aes(x = time)) +
    geom_line(aes(y = testing_reg1), col = theta99_purple, size = my_linesize) +
    geom_line(aes(y = notesting_reg1), col = "black", size = my_linesize) +
    ylab("Infected (#)") +
    xlab("Time (days)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 250)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2500)) + # 2500 for R0 = 6
    alllabels_theme
  
  #* Panel B - infected over time for phi = 0.75 ####
  this_phi <- 0.75
  
  df_notesting_reg2 <- run_leaky_model(this_phi, this_VE_I, this_VE_S, notesting_theta, this_q,
                                       psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                       H_I = this_H_I, H_S = this_H_S)
  df$notesting_reg2 <- df_notesting_reg2$I_v + df_notesting_reg2$I_u + df_notesting_reg2$I_x + df_notesting_reg2$I_h
  
  df_testing_reg2 <- run_leaky_model(this_phi, this_VE_I, this_VE_S, testing_theta, this_q,
                                     psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                     H_I = this_H_I, H_S = this_H_S)
  df$testing_reg2 <- df_testing_reg2$I_v + df_testing_reg2$I_u + df_testing_reg2$I_x + df_testing_reg2$I_h
  
  B <- ggplot(df, aes(x = time)) +
    geom_line(aes(y = testing_reg2), col = theta99_purple, size = my_linesize) +
    geom_line(aes(y = notesting_reg2), col = "black", size = my_linesize) +
    ylab("Infected (#)") +
    xlab("Time (days)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 250)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2500)) +
    onlyx_theme
  
  
  #* Panel C - infected over time for phi = 0.95 ####
  this_phi <- 0.95
  
  df_notesting_reg3 <- run_leaky_model(this_phi, this_VE_I, this_VE_S, notesting_theta, this_q,
                                       psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                       H_I = this_H_I, H_S = this_H_S)
  df$notesting_reg3 <- df_notesting_reg3$I_v + df_notesting_reg3$I_u + df_notesting_reg3$I_x + df_notesting_reg3$I_h
  
  df_testing_reg3 <- run_leaky_model(this_phi, this_VE_I, this_VE_S, testing_theta, this_q,
                                     psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                     H_I = this_H_I, H_S = this_H_S)
  df$testing_reg3 <- df_testing_reg3$I_v + df_testing_reg3$I_u + df_testing_reg3$I_x + df_testing_reg3$I_h
  
  C <- ggplot(df, aes(x = time)) +
    geom_line(aes(y = testing_reg3), col = theta99_purple, size = my_linesize) +
    geom_line(aes(y = notesting_reg3), col = "black", size = my_linesize) +
    ylab("Infected (#)") +
    xlab("Time (days)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 250)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2500)) +
    onlyx_theme
  
  #* Panel D - Reff over phi ####
  df <- data.frame(phi = phi_vec)
  
  df$Reff_notesting <- sapply(phi_vec, compute_Reff,
                              VE_I = this_VE_I, VE_S = this_VE_S,
                              theta = notesting_theta, q = this_q,
                              psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                              H_I = this_H_I, H_S = this_H_S)
  
  df$Reff_testing <- sapply(phi_vec, compute_Reff,
                            VE_I = this_VE_I, VE_S = this_VE_S,
                            theta = testing_theta, q = this_q,
                            psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                            H_I = this_H_I, H_S = this_H_S)
  
  D <- ggplot(df, aes(x = phi*100)) +
    geom_line(aes(y = Reff_testing), col = theta99_purple, size = my_linesize) +
    geom_line(aes(y = Reff_notesting), col = "black", size = my_linesize) +
    geom_line(aes(y = 1), col = mylightgray, linetype = "dashed", size = my_linesize) +
    ylab(expression(R[eff])) +
    xlab("Population vaccination rate (%)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3.5)) +
    alllabels_theme
}

ggarrange(A, NULL, B, NULL, C, D, NULL,
          labels = c("a",NA,"     b",NA,"     c","  d",NA),
          nrow = 1,
          ncol = 7,
          align = "hv",
          widths = c(1.03,-.1, 1,-.1, 1., 1.03,0.1),
          heights = c(1, 1, 1, 1))

ggsave("Fig4.pdf", device = cairo_pdf, width = 10, height = 3)