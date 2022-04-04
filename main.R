# Main.R - runs SEIR model
#        - generates main text line figures (fig 1, 4, 7)
# Written by: Kate Bubar and Casey Middleton

# setup.R works correctly if you've already installed fonts. if not, go to setup.R line 22.
source("setup.R")

testing_everyone <- 0 # if 0, just testing unvacc. when implementing testing

# _____________________________________________________________________
# FIGURE 1 - infections with no testing ####
# _____________________________________________________________________
# for loop to run fig 1

for (i in 1){
  #* Panel B - infected over time ####
  this_theta <- 0 # no testing
  this_q <- 0 # well-mixed

  df <- run_leaky_model(this_phi, this_VE_I, this_VE_S, this_theta, this_q,
                          psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                          H_I = this_H_I, H_S = this_H_S)

  B <- ggplot(df, aes(x = time)) +
    geom_line(aes(y = I_v + I_h), col = mylightgray, size = my_linesize) +
    geom_line(aes(y = I_u + I_x), col = mygray, size = my_linesize, linetype = "longdash")  +
    geom_line(aes(y = I_v + I_u + I_x + I_h), col = myblack, size = my_linesize) +
    ylab("Infected (#)") +
    xlab("Time (days)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 200)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) + # 2500 for R0 = 6
    alllabels_theme
  
  #* Panel C - hospitalized over time ####
  list_hosp <- compute_new_daily_hosp(this_phi, this_VE_I, this_VE_S, this_VE_P,
                                      this_theta, this_q,
                                      this_psi, this_X_I, this_X_S, this_X_P,
                                      this_H_I, this_H_S, this_H_P)
  
  df$hosp_u <- unlist(list_hosp[[1]])
  df$hosp_v <- unlist(list_hosp[[2]])
  
  C <- ggplot(df, aes(x = time)) +
    geom_line(aes(y = hosp_v), col = mylightgray, size = my_linesize) +
    geom_line(aes(y = hosp_u), col = mygray, size = my_linesize, linetype = "longdash")  +
    geom_line(aes(y = hosp_u + hosp_v), col = myblack, size = my_linesize) +
    ylab("New daily hosp. (#)") +
    xlab("Time (days)") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 200)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,2)) + # 2500 for R0 = 6
    alllabels_theme
  
  
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
  

  #* Panel E - total infections and breakthrough cases over phi ####
  df <- data.frame(phi = phi_vec)
  baseline_inf <- compute_tot_infections(phi = 0, this_VE_I, this_VE_S, this_theta, this_q,
                                         psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                         H_I = this_H_I, H_S = this_H_S )
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
  
  E <- ggplot(df, aes(x=phi*100)) +
    geom_line(aes(y=breakthrough), col = mylightgray, size = my_linesize) +
    geom_line(aes(y=tot_infections/baseline_inf*100), col = myblack, size = my_linesize) +
    ylab("Percentage") +#"Total infected (%)") +
    xlab("Population vacc. rate (%)") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
                       # sec.axis = sec_axis(~., name="Breakthrough infections (%)")) +
    scale_x_continuous(expand = c(0, 0)) +
    alllabels_theme +
    theme(#axis.title.y.right = element_text(color = mylightgray),
          #axis.title.y.left = element_text(color = myblack),
          #axis.title.x.bottom = element_blank(),
          #axis.text.x.bottom = element_blank(),
          plot.margin=unit(c(6, 2, 5.5, 2), "pt")) # top, right, bottom, left

  Reff_1 <- min(which(df$Reff <= 1),500) - 1
  if (Reff_1 <= 100){
    # C <- C + geom_vline(xintercept = Reff_1, alpha = 0.5, linetype = "dashed", size = 0.5)
    E <- E + geom_point(aes(x = Reff_1, y = 0), shape = 20, size = 0.4)
  }
  inf_transition <- min(which(df$breakthrough >= 50)) - 1
  E <- E + geom_vline(xintercept = inf_transition, alpha = 1, linetype = "dashed", size = 0.5, col = infcolor)

  print(paste0("Percentage of infections in unvaccinated population with 58% vaccination rate: ",100-df[df$phi==0.58,]$breakthrough))

  #* Panel F - total infections and breakthrough cases over phi ####
  baseline_hosp <- compute_tot_hospitalizations(phi = 0, this_VE_I, this_VE_S, this_VE_P, infection_hosp_rate_delta, 
                                                this_theta, this_q,
                                                psi = this_psi, X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
  df$tot_hosp <- sapply(phi_vec, compute_tot_hospitalizations,
                        VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P,
                        infection_hosp_rate = infection_hosp_rate_delta,
                        theta = this_theta, q = this_q,
                        psi = this_psi, X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                        H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
  
  df$breakthrough_hosp <- sapply(phi_vec, compute_percent_breakthrough_hosp,
                                 VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P,
                                 infection_hosp_rate = infection_hosp_rate_delta,
                                 theta = this_theta, q = this_q,
                                 psi = this_psi, X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                 H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
  
  F <- ggplot(df, aes(x=phi*100)) +
    geom_line(aes(y=breakthrough_hosp), col = mylightgray, size = my_linesize) +
    geom_line(aes(y=tot_hosp/baseline_hosp*100), col = myblack, size = my_linesize) +
    ylab("Percentage") +#"Total infected (%)") +
    xlab("Population vacc. rate (%)") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
    # sec.axis = sec_axis(~., name="Breakthrough infections (%)")) +
    scale_x_continuous(expand = c(0, 0)) +
    alllabels_theme +
    theme(#axis.title.y.right = element_text(color = mylightgray),
      #axis.title.y.left = element_text(color = myblack),
      #axis.title.x.bottom = element_blank(),
      #axis.text.x.bottom = element_blank(),
      plot.margin=unit(c(6, 2, 5.5, 2), "pt")) # top, right, bottom, left
  
  Reff_1 <- min(which(df$Reff <= 1),500) - 1
  if (Reff_1 <= 100){
    # C <- C + geom_vline(xintercept = Reff_1, alpha = 0.5, linetype = "dashed", size = 0.5)
    F <- F + geom_point(aes(x = Reff_1, y = 0), shape = 20, size = 0.4)
  }
  hosp_transition <- min(which(df$breakthrough_hosp >= 50)) - 1
  F <- F + geom_vline(xintercept = hosp_transition, alpha = 1, linetype = "dashed", size = 0.5, col = hospcolor)
  
  print(paste0("Percentage of hospitalizations in unvaccinated population with 58% vaccination rate: ",
               100-df[df$phi==0.58,]$breakthrough_hosp))
  
  
  #* Panel G - cumulative transmission mode over phi ####
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

  G <- ggplot(df_toplot, aes(x=phi*100, y=value*100, color = variable)) +
    geom_line(size = my_linesize) +
    ylab(paste("Transmission mode (%) ")) +
    xlab(expression("Population vacc. rate (%)")) +
    labs(fill = "") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100.1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_color_manual(values = c(mygray, mylightgray, mydarkteal, mylightteal, mylightorange,
                                  mydarkorange)) +
    #labels = c("v to v", "v to u", "u to v", "u to u")) +
    theme(legend.position = "none")

  if (Reff_1 <= 100){
    # E <- E + geom_vline(xintercept = Reff_1, alpha = 0.5, linetype = "dashed", size = 0.5)
    G <- G + geom_point(aes(x = Reff_1, y = 0), shape = 20, size = 0.4, color = "black")
  }
  trans_transition <- min(which(df$total_by_u < df$total_by_v)) - 1
  G <- G + geom_vline(xintercept = trans_transition, alpha = 1, linetype = "dashed", size = 0.5, col = transmcolor)
}

# export as cairo_pdf,7x5.5in
ggarrange(NULL, NULL, NULL, NULL, NULL, NULL,
          B, NULL, C, NULL, D, NULL, NULL, NULL,
          NULL, NULL, NULL, NULL,
          E, NULL, F, NULL, G, NULL,
          labels = c(rep(NA, 6),
                     "b", NA, "c", NA, "d", NA,
                     rep(NA, 6),
                     "e", NA, " f", NA, "g", NA),
          nrow = 4,
          ncol = 6,
          align = "hv",
          widths = c(1, 0, 1, 0, 1, 0.05),
          heights = c(0.05, 1, 0.05, 1),
          label.y = 1.04)

ggsave("Fig1.pdf", device = cairo_pdf, width = 8, height = 5.5)


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


# _____________________________________________________________________
# FIGURE 7 - transition points ####
# Show uncertainty in transition points over VE and testing scenarios
# _____________________________________________________________________

# read in data
baseline_df <- readRDS("dataframes/df_Fig7_baseline.RData")
waning_df <- readRDS("dataframes/df_Fig7_waning.RData")
boosted_df <- readRDS("dataframes/df_Fig7_boosted.RData")
omicron_df <- readRDS("dataframes/df_Fig7_omicron.RData")
df <- readRDS("dataframes/df_Fig7_R04.RData")

# or run simulations
phi_vec <- seq(0.5, 1, by = 0.01)
psi_vec <- seq(0, 1, by = 0.01)
testing_everyone <- 0 # unvaccinated only testing

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
  saveRDS(waning_df,"df_Fig7_waning.RData")
  saveRDS(baseline_df,"df_Fig7_baseline.RData")
  saveRDS(boosted_df,"df_Fig7_boosted.RData")
  saveRDS(omicron_df,"df_Fig7_omicron.RData")

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
  saveRDS(df,"df_Fig7.RData")

}

# Plot Figure 7

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


fig7 <- ggarrange(p_inf_transition, NULL, p_trnsmsn_transition, NULL, p_hosp_transition, NULL,
                  widths = c(1.02, -0.05, 1, -0.05, 1, 0.1),
                  labels = c('   a ', NA, '  b', NA, '  c', NA),
                  ncol = 6,
                  label.y = 0.96,
                  align = "hv")
fig7

ggsave("Fig7.pdf", fig7, device = cairo_pdf, width = 7.5, height = 3.5)
