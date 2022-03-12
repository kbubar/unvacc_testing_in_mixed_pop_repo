
source("setup.R") 

this_theta <- 0
testing_everyone <- 0 # if 0, just testing unvacc. when implementing testing

list_hosp <- compute_hospitalizations(this_phi, this_VE_I, this_VE_S, this_VE_P, infection_hosp_rate_delta, this_theta, this_q,
                                      psi = this_psi, X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                      H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)

print(compute_tot_infections(this_phi, this_VE_I, this_VE_S, this_theta, this_q,
                             psi = this_psi, X_I = this_X_I, X_S = this_X_S, 
                             H_I = this_H_I, H_S = this_H_S))

df <- data.frame(hosp = list_hosp$v_hosp,
                 u_hosp = list_hosp$u_hosp,
                 x_hosp = list_hosp$x_hosp,
                 h_hosp = list_hosp$h_hosp)

barplot(c(df$u_hosp, df$x_hosp, df$v_hosp, df$h_hosp),
        names.arg = c("unvacc", "exper.", "vacc", "hybrid"),
        ylab = "Total hospitalizations",
        main = "phi = 58%, psi = 35%")

# _____________________________________________________________________
# RECREATE FIGURE 1C with hospitalizations: ####
#*
#* I: Get data ####
# _____________________________________________________________________

for (i in 1){
  #* Panel B - infected over time ####
  this_theta <- 0 # no testing
  this_q <- 0 # well-mixed
 
  #* Panel C - total infections and breakthrough cases over phi ####
  df <- data.frame(phi = phi_vec)
  df$tot_hosp <- sapply(phi_vec, compute_tot_hospitalizations,
                              VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P,
                              infection_hosp_rate = infection_hosp_rate_delta,
                              theta = this_theta, q = this_q,
                              psi = this_psi, X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                              H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
  
  df$Reff <- sapply(phi_vec, compute_Reff,
                    VE_I = this_VE_I, VE_S = this_VE_S,
                    theta = this_theta, q = this_q,
                    psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                    H_I = this_H_I, H_S = this_H_S)
  
  df$breakthrough <- sapply(phi_vec, compute_percent_breakthrough_hosp,
                            VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P,
                            infection_hosp_rate = infection_hosp_rate_delta,
                            theta = this_theta, q = this_q,
                            psi = this_psi, X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                            H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
  
  C <- ggplot(df, aes(x=phi*100)) +
    geom_line(aes(y=breakthrough), col = mylightgray, size = my_linesize) +
    geom_line(aes(y=tot_hosp/N*100), col = myblack, size = my_linesize) +
    ylab("Percentage") +#"Total infected (%)") +
    xlab("Population vaccination rate (%)") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 300)) +
    # sec.axis = sec_axis(~., name="Breakthrough infections (%)")) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(axis.title.y.right = element_text(color = mylightgray),
          axis.title.y.left = element_text(color = myblack),
          #axis.title.x.bottom = element_blank(),
          #axis.text.x.bottom = element_blank(),
          plot.margin=unit(c(6, 2, 5.5, 2), "pt")) # top, right, bottom, left
  
  Reff_1 <- min(which(df$Reff <= 1),500) - 1
  if (Reff_1 <= 100){
    # C <- C + geom_vline(xintercept = Reff_1, alpha = 0.5, linetype = "dashed", size = 0.5)
    C <- C + geom_point(aes(x = Reff_1, y = 0), shape = 20, size = 0.4)
  }
  inf_transition <- min(which(df$breakthrough >= 50)) - 1
  C <- C + geom_vline(xintercept = inf_transition, alpha = 1, linetype = "dashed", size = 0.5, col = mylightgreen)
  
  print(paste0("Percentage of infections in unvaccinated population with 58% vaccination rate: ",100-df[df$phi==0.58,]$breakthrough))

}


# _____________________________________________________________________
# RECREATE FIGURE 2A, B with hospitalizations: ####
#*
#* I: Get data ####
# _____________________________________________________________________

ptm <- proc.time()
phi_vec <- seq(0, 1, by = 0.05) # fine grid : by = 0.01 (~20 min)
psi_vec <- seq(0, 1, by = 0.05)
df <- expand.grid(phi = phi_vec, psi = psi_vec)

df$Reff <- NA
df$tot_hosp <- NA
df$breakthrough <- NA
df$dom_transmission <- NA
df$tot_infections <- NA
df$breakthrough_hosp <- NA

this_VE_I <- omicron_VE_I
this_VE_S <- omicron_VE_S
this_VE_P <- omicron_VE_P
this_H_I  <- omicron_H_I
this_H_S  <- omicron_H_S
this_H_P  <- omicron_H_P

for (i in 1:dim(df)[1]){
  list_hosp <- compute_hospitalizations(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P, 
                                        infection_hosp_rate = infection_hosp_rate_omicron,
                                        theta = 0, q = this_q, 
                                        df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                        H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
  df$tot_hosp[i] <- unlist(list_hosp[[1]])
  
  df$Reff[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                             theta = 0, q = this_q,
                             df$psi[i], X_I = this_X_I, X_S = this_X_S,
                             H_I = this_H_I, H_S = this_H_S)
  
  df$breakthrough[i] <- compute_percent_breakthrough_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                                theta = 0, q = this_q,
                                                                df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                H_I = this_H_I, H_S = this_H_S)
  
  df$dom_transmission[i] <- (compute_dominant_transmission(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                           theta = 0, q = this_q,
                                                           df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                           H_I = this_H_I, H_S = this_H_S))*100
  
  df$breakthrough_hosp[i] <- compute_percent_breakthrough_hosp(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P, 
                                                               infection_hosp_rate = infection_hosp_rate_omicron,
                                                               theta = 0, q = this_q, 
                                                               df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                               H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
  
}


#saveRDS(df,file="df_Fig2_baseline.RData")

proc.time() - ptm

p_Reff <- ggplot(df, aes(x = phi*100, y = psi*100, z = Reff))+ #, colour = ..level..)) + 
  geom_tile(aes(fill = tot_hosp)) +
  geom_contour(breaks = 1:R0, size = 0.4, color = "white") +
  geom_text_contour(breaks = 1:R0, color = "white", rotate = FALSE, skip = 0) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") + 
  #ggtitle(expression(R[eff])) +
  ggtitle("Total hospitalizations") +
  scale_fill_viridis(option="viridis", limits = c(0, max(df$tot_hosp))) +
  coord_fixed(1) + 
  labs(fill = "") +
  theme(legend.text = element_text(size = 11), 
        legend.spacing.x = unit(0.7, 'cm')) 

# plot % hosp in the unvaccinated
p_hosp <- ggplot(df, aes(x = phi*100, y = psi*100, z = breakthrough_hosp)) + 
  geom_tile(aes(fill = 100 - breakthrough)) +
  stat_contour(breaks = c(50), size = 0.4, col = "white") + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") + 
  ggtitle("% of hospitalizations among\nunvaccinated individuals") +
  labs(fill = "") +#"Percent\nhosp\nin vacc.") +
  theme(legend.text = element_text(size = 11), 
        legend.title = element_text(size = 11),
        legend.title.align = 0.5,
        legend.spacing.x = unit(0.5, 'cm')) +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno")) + 
  coord_fixed(1)

num_legend <- get_legend(p_Reff)
p_Reff <- p_Reff + theme(legend.position = "none")

percent_legend <- get_legend(p_hosp)
p_transmission <- p_transmission + theme(legend.position = "none")

plot_a <- ggarrange(p_Reff,
                    labels = c('a  '),
                    label.y = 0.92)

plot_b <- ggarrange(p_hosp,
                    labels = c('b  '),
                    label.y = 0.92)

fig2 <- arrangeGrob(plot_a, num_legend, plot_b,
                    nrow = 1,
                    ncol = 3,
                    widths = c(1, 0.4, 1))


ggsave("Fig2.pdf", fig2, device = cairo_pdf, width = 10, height = 3)
ggsave("Fig2.svg", fig2, device = svg, width = 10, height = 3)
# _____________________________________________________________________
# FIGURE 5: ####
# Total infections averted and percent reduction in infections for 
# all three VE scenarios (waning, baseline, boosted) and
# both testing scenarios (weekly, 50% and 99% compliance)
#
#*
#* I: Get data ####
# _____________________________________________________________________
waningdf <- readRDS("df_suppFig5_waning_R04_hosp.RData")
baselinedf <- readRDS("df_suppFig5_baseline_R04_hosp.RData")
boosteddf <- readRDS("df_suppFig5_boosted_R04_hosp.RData")

ptm <- proc.time()

phi_vec <- seq(0, 1, by = 0.05) # fine grain: by = 0.01
psi_vec <- seq(0, 1, by = 0.05)
df <- expand.grid(phi = phi_vec, psi = psi_vec)

df$Reff <- NA
df$Reff_99 <- NA
df$Reff_50 <- NA
df$tothosp_notesting <- NA
df$tothosp_99 <- NA
df$tothosp_50 <- NA

this_VE_I <- baseline_VE_I
this_VE_S <- baseline_VE_S
this_VE_P <- baseline_VE_P
this_H_I  <- baseline_H_I
this_H_S  <- baseline_H_S
this_H_P  <- baseline_H_P

for (i in 1:dim(df)[1]){
  df$Reff[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                             theta = 0, q = this_q,
                             df$psi[i], X_I = this_X_I, X_S = this_X_S,
                             H_I = this_H_I, H_S = this_H_S)
  
  df$Reff_99[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                theta = theta_99, q = this_q,
                                df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                H_I = this_H_I, H_S = this_H_S)
  df$Reff_50[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                theta = theta_50, q = this_q,
                                df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                H_I = this_H_I, H_S = this_H_S)
  
  df$tothosp_notesting[i] <- compute_tot_hospitalizations(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P, 
                                        infection_hosp_rate = infection_hosp_rate_delta,
                                        theta = 0, q = this_q, 
                                        df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                        H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
  
  df$tothosp_99[i] <- compute_tot_hospitalizations(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P, 
                                                         infection_hosp_rate = infection_hosp_rate_delta,
                                                         theta = theta_99, q = this_q, 
                                                         df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                         H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
  
  df$tothosp_50[i] <- compute_tot_hospitalizations(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P, 
                                                  infection_hosp_rate = infection_hosp_rate_delta,
                                                  theta = theta_50, q = this_q, 
                                                  df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                  H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
  
}

df$percent_reduc_hosp_99 <- (df$tothosp_notesting - df$tothosp_99)/df$tothosp_notesting*100
df$percent_reduc_hosp_50 <- (df$tothosp_notesting - df$tothosp_50)/df$tothosp_notesting*100

proc.time() - ptm
#saveRDS(df,file="df_suppFig5_waning_R04_hosp.RData")

#* II: Plot fig5 ####
for (i in 1:3) {
  if (i == 1) {
    df <- waningdf
  } else if (i==2) {
    df <- baselinedf
  } else {
    df <- boosteddf
  }
  
  percentreduc50 <- ggplot(df, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) +
    geom_tile(aes(fill = percent_reduc_hosp_50_uonly)) +
    geom_contour(aes(z = Reff_50_uonly), breaks = 1, size = 0.6, color = "white") +
    geom_contour(aes(z = Reff), breaks = 1, size = 0.6, col = "white", linetype = "longdash") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    ylab("") +#"Infection-acquired immunity (%)") +
    xlab("") +# xlab("Population vaccination rate (%)") +
    ggtitle("") +
    scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) +
    coord_fixed(1) +
    theme(legend.position = "none")
  
  percentreduc99 <- ggplot(df, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) +
    geom_tile(aes(fill = percent_reduc_hosp_99_uonly)) +
    geom_contour(aes(z = Reff_99_uonly), breaks = 1, size = 0.6, col = "white") +
    geom_contour(aes(z = Reff), breaks = 1, size = 0.6, col = "white", linetype = "longdash") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    ylab("Infection-acquired immunity (%)") +
    xlab("") +# xlab("Population vaccination rate (%)") +
    ggtitle("Weekly testing, 99% compliance", "% reduction in infections due to testing") +
    scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) +
    coord_fixed(1) +
    labs(fill = "") +
    theme(axis.title.y = element_blank(),
          plot.title = element_blank(),
          plot.subtitle = element_blank()) #element_text(hjust = 0.5))
  
  percent_legend <- get_legend(percentreduc99)
  
  if (i == 1){
    percentreduc50_waning <- percentreduc50 + onlyy_theme + ggtitle("Waning/low VE")
    percentreduc99_waning <- percentreduc99 + theme(legend.position = "none")
  } else if (i == 2){
    percentreduc50_baseline <- percentreduc50 + nolabels_theme + ggtitle("Baseline VE")
    percentreduc99_baseline <- percentreduc99 + onlyx_theme + 
      theme(legend.position = "none", plot.title = element_blank()) + 
      xlab("Population vaccination rate (%)")
  } else {
    percentreduc50_boosted <- percentreduc50 + nolabels_theme + ggtitle("Boosted/high VE")
    percentreduc99_boosted <- percentreduc99 + onlyx_theme + theme(legend.position = "none", plot.title = element_blank())
  }
}

panels <- ggarrange(percentreduc50_waning, NULL, percentreduc50_baseline, NULL, percentreduc50_boosted,
                    NULL, NULL, NULL, NULL, NULL,
                    percentreduc99_waning, NULL, percentreduc99_baseline, NULL, percentreduc99_boosted,
                    nrow = 3, ncol = 5,
                    align = "hv",
                    widths = c(1, -0.14, 1, -0.14, 1),
                    heights = c(1, -0.22, 1),
                    labels = c("a", NA, "    b", NA,"    c",
                               NA, NA, NA, NA, NA,
                               "d", NA, "    e", NA, "    f"),
                    label.y = 0.88)

lay <- rbind(c(1, 2))

fig5 <- arrangeGrob(panels, percent_legend, layout_matrix = lay,
                    widths = c(3, 0.5), 
                    left = c("Infection-acquired immunity (%)"))

ggsave("suppFig5_R04_hosp.pdf", fig5, device = cairo_pdf, width = 8, height = 5)
#ggsave("suppFig5.svg", fig5, device = svg, width = 8, height = 5)


###########################################################################################3
###########################################################################################3
##  Casey Proposed Figures
###########################################################################################3
###########################################################################################3

# Fig A: 1% increase in vaccine allocation decreases hospitalization more than infections

baseline_hosp <- compute_tot_hospitalizations(phi = 0, this_VE_I, this_VE_S, this_VE_P, infection_hosp_rate_delta, this_theta, this_q,
                                      psi = 0, X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                      H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
baseline_inf <- compute_tot_infections(phi = 0, this_VE_I, this_VE_S, this_theta, this_q,
                                          psi = 0, X_I = this_X_I, X_S = this_X_S,
                                          H_I = this_H_I, H_S = this_H_S )

df <- data.frame(phi = phi_vec)

df$tot_hosp <- sapply(phi_vec, compute_tot_hospitalizations,
                      VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P,
                      infection_hosp_rate = infection_hosp_rate_delta,
                      theta = this_theta, q = this_q,
                      psi = 0, X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                      H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
df$tot_inf <- sapply(phi_vec, compute_tot_infections,
                      VE_I = this_VE_I, VE_S = this_VE_S, 
                      theta = this_theta, q = this_q,
                      psi = 0, X_I = this_X_I, X_S = this_X_S,
                      H_I = this_H_I, H_S = this_H_S)
df$breakthrough <- sapply(phi_vec, compute_percent_breakthrough_hosp,
                          VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P,
                          infection_hosp_rate = infection_hosp_rate_delta,
                          theta = this_theta, q = this_q,
                          psi = this_psi, X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                          H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)

df$tot_hosp_prevented = (1 - df$tot_hosp/baseline_hosp)
df$tot_inf_prevented = (1 - df$tot_inf/baseline_inf)

A <- ggplot(df, aes(x=phi*100)) +
  geom_line(aes(y=tot_hosp_prevented*100), col = mylightgray, size = my_linesize) +
  geom_line(aes(y=tot_inf_prevented*100), col = myblack, size = my_linesize) +
  ylab("Percent Reduction") +#"Total infected (%)") +
  xlab("Population vaccination rate (%)") +
  ggtitle("Vaccination Impact on Infections\n and Hospitalizations") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  # sec.axis = sec_axis(~., name="Breakthrough infections (%)")) +
  scale_x_continuous(expand = c(0, 0))
  #theme(axis.title.y.right = element_text(color = mylightgray),
  #      axis.title.y.left = element_text(color = myblack),
        #axis.title.x.bottom = element_blank(),
        #axis.text.x.bottom = element_blank(),
  #      plot.margin=unit(c(6, 2, 5.5, 2), "pt")) # top, right, bottom, left

hosp_transition <- min(which(df$breakthrough >= 50)) - 1
A <- A + geom_vline(xintercept = hosp_transition, alpha = 1, linetype = "dashed", size = 0.5, col = mylightgreen)








