source("setup.R")

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