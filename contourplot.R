# contourplot.R - generates heatmaps used in main text
#               - main text figs 2, 3, 5, 6  
# Written by: Kate Bubar and Casey Middleton

source("setup.R") 


# _____________________________________________________________________
# FIGURE 2: ####
# Reff, % infections in the unvaccinated and % infections driven by the unvaccinated over phi and psi
#
#*
#* I: Get data ####
# _____________________________________________________________________

# Either read in the corresponding RDS file
df <- readRDS("df_fig2_baseline.RData")
#df <- readRDS("df_suppfig2_baseline_R06.RData")

# or run the model 
ptm <- proc.time()
phi_vec <- seq(0, 1, by = 0.01) # fine grid : by = 0.01 (~20 min)
psi_vec <- seq(0, 1, by = 0.01)
df <- expand.grid(phi = phi_vec, psi = psi_vec)

df$Reff <- NA
df$breakthrough <- NA
df$dom_transmission <- NA
df$tot_infections <- NA

this_VE_I <- baseline_VE_I
this_VE_S <- baseline_VE_S
this_H_I  <- baseline_H_I
this_H_S  <- baseline_H_S

for (i in 1:dim(df)[1]){
  df$tot_infections[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,  
                                              theta = 0, q = this_q, 
                                              df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                              H_I = this_H_I, H_S = this_H_S)
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
}

#saveRDS(df,file="df_Fig2_baseline.RData")

proc.time() - ptm

# _____________________________________________________________________
#* II: Plot fig2 ####
# _____________________________________________________________________

# plot Reff
p_Reff <- ggplot(df, aes(x = phi*100, y = psi*100, z = Reff))+ #, colour = ..level..)) + 
  geom_tile(aes(fill = tot_infections)) +
  geom_contour(breaks = 1:R0, size = 0.4, color = "white") +
  geom_text_contour(breaks = 1:R0, color = "white", rotate = FALSE, skip = 0) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") + 
  #ggtitle(expression(R[eff])) +
  ggtitle("Total infections") +
  scale_fill_viridis(option="viridis", limits = c(0, N)) +
  coord_fixed(1) + 
  labs(fill = "") +
  theme(legend.text = element_text(size = 11), 
        legend.spacing.x = unit(0.7, 'cm')) 

# plot % infections in the unvaccinated
p_infection <- ggplot(df, aes(x = phi*100, y = psi*100, z = breakthrough)) + 
  geom_tile(aes(fill = 100 - breakthrough)) +
  stat_contour(breaks = c(50), size = 0.4, col = "white") + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + # ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") + 
  ggtitle("% of infections among\nunvaccinated individuals") +
  labs(fill = "Percent\ninfections\nin vacc.") +
  theme(legend.text = element_text(size = 11), 
        legend.title = element_text(size = 11),
        legend.title.align = 0.5,
        legend.spacing.x = unit(0.5, 'cm'), 
        legend.position = "none") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno")) + 
  coord_fixed(1)

# plot dominant transmission
p_transmission <- ggplot(df, aes(x = phi*100, y = psi*100, z = dom_transmission)) + 
  geom_tile(aes(fill = dom_transmission)) +
  stat_contour(breaks = c(50), size = 0.4, color = "white") + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + # ylab("Infection-acquired immunity (%)") +
  xlab("") + # xlab("Population vaccination rate (%)") + 
  ggtitle("% of transmission from\nunvaccinated individuals") + 
  labs(fill = "")+ #"Percent of\ntransmission\nby unvacc.") +
  theme(legend.text = element_text(size = 11), 
        legend.title = element_text(size = 11),
        legend.title.align = 0.5,
        legend.spacing.x = unit(0.45, 'cm')) +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"))+ 
  coord_fixed(1)

num_legend <- get_legend(p_Reff)
p_Reff <- p_Reff + theme(legend.position = "none")

percent_legend <- get_legend(p_transmission)
p_transmission <- p_transmission + theme(legend.position = "none")

plot_a <- ggarrange(p_Reff,
                    labels = c('a  '),
                    label.y = 0.92)

plot_b <- ggarrange(p_infection,
                     labels = c('b'),
                     label.y = 0.92)

plot_c <- ggarrange(p_transmission,
                    labels = c('c'),
                    label.y = 0.92)

fig2 <- arrangeGrob(plot_a, num_legend, plot_b, plot_c, percent_legend,
                    nrow = 1,
                    ncol = 5,
                    widths = c(1, 0.4, 1, 1, 0.4))


ggsave("Fig2.pdf", fig2, device = cairo_pdf, width = 10, height = 3)
ggsave("Fig2.svg", fig2, device = svg, width = 10, height = 3)

# To find transition points:
infection_transitions <- df[df$breakthrough >= 50,]  %>% group_by(psi) %>% summarize(phi=min(phi))
transmission_transitions <- df[df$dom_transmission < 50,]  %>% group_by(psi) %>% summarize(phi=min(phi))
print(paste0("Infection transition point: ",min(infection_transitions$phi), " - ",max(infection_transitions$phi)))
print(paste0("Transmission transition point: ",min(transmission_transitions$phi), " - ",max(transmission_transitions$phi)))

# _____________________________________________________________________
# FIGURE 3: ####
# Reff and transition lines three scenarios
#
#*
#* I: Get data ####
# _____________________________________________________________________

# Same df as figure 2
baselinedf <- readRDS("df_fig2_baseline.RData")
waningdf <- readRDS("df_fig2_waning.RData")
boosteddf <- readRDS("df_fig2_boosted.RData")
omicrondf <- readRDS("df_fig2_omicron.RData")

# _____________________________________________________________________
#* II: Plot fig3 ####
# _____________________________________________________________________

p_waning <- ggplot(waningdf, aes(x = phi*100, y = psi*100))+ #, colour = ..level..)) + 
  geom_contour(aes(z = dom_transmission), breaks = 50, size = my_linesize, color = mydarkgreen) +
  geom_contour(aes(z = breakthrough), breaks = 50, size = my_linesize, color = mylightgreen) +
  geom_contour(aes(z = Reff),breaks = 1, size = my_linesize, color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") + 
  ggtitle("Waning/low VE") +
  coord_fixed(1) + 
  theme(legend.position = "none") 

p_baseline <- ggplot(baselinedf, aes(x = phi*100, y = psi*100))+ #, colour = ..level..)) + 
  geom_contour(aes(z = dom_transmission), breaks = 50, size = my_linesize, color = mydarkgreen) +
  geom_contour(aes(z = breakthrough), breaks = 50, size = my_linesize, color = mylightgreen) +
  geom_contour(aes(z = Reff),breaks = 1, size = my_linesize, color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("") +
  xlab("Population vaccination rate (%)") + 
  ggtitle("Baseline VE") +
  coord_fixed(1) + 
  theme(legend.position = "none", 
        axis.title.y = element_blank()) 

p_boosted <- ggplot(boosteddf, aes(x = phi*100, y = psi*100))+ #, colour = ..level..)) + 
  geom_contour(aes(z = dom_transmission), breaks = 50, size = my_linesize, color = mydarkgreen) +
  geom_contour(aes(z = breakthrough), breaks = 50, size = my_linesize, color = mylightgreen) +
  geom_contour(aes(z = Reff),breaks = 1, size = my_linesize, color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("") +
  xlab("Population vaccination rate (%)") + 
  ggtitle("Boosted/high VE") +
  coord_fixed(1) + 
  theme(legend.position = "none", 
        axis.title.y = element_blank()) 

p_omicron <- ggplot(omicrondf, aes(x = phi*100, y = psi*100))+ #, colour = ..level..)) + 
  geom_contour(aes(z = dom_transmission), breaks = 50, size = my_linesize, color = mydarkgreen) +
  geom_contour(aes(z = breakthrough), breaks = 50, size = my_linesize, color = mylightgreen) +
  geom_contour(aes(z = Reff),breaks = 1, size = my_linesize, color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("") +
  xlab("Population vaccination rate (%)") + 
  ggtitle("Plausible Omicron") +
  coord_fixed(1) + 
  theme(legend.position = "none", 
        axis.title.y = element_blank()) 

fig3 <- ggarrange(p_waning, p_baseline, p_boosted, p_omicron,NULL,
                     nrow = 1,
                     align = "hv",
                     labels = c("a", "b", "c", "d",NULL),
                     widths = c(1,1,1,1,0.1))

ggsave("Fig3.pdf", fig3, device = cairo_pdf, width = 10, height = 3)
ggsave("Fig3.svg", fig3, device = svg, width = 10, height = 3)

# _____________________________________________________________________
# FIGURE 5: ####
# Total infections averted and percent reduction in infections for 
# all three VE scenarios (waning, baseline, boosted) and
# both testing scenarios (weekly, 50% and 99% compliance)
#
#*
#* I: Get data ####
# _____________________________________________________________________

# Either read in the corresponding RDS file
# Dataframes for main text figure 5
# waningdf <- readRDS("df_Fig5_waning.RData")
# baselinedf <- readRDS("df_Fig5_baseline.RData")
boosteddf <- readRDS("df_Fig5_boosted.RData")

# Dataframes for supp text figure 5 - R0 = 6
# waningdf <- readRDS("df_suppFig5_waning_R06.RData")
# baselinedf <- readRDS("df_suppFig5_baseline_R06.RData")
# boosteddf <- readRDS("df_suppFig5_boosted_R06.RData")

# Dataframes for supp text figure 5 - R0 = 4, testing everyone
waningdf <- readRDS("df_suppFig5_waning_R04_testeveryone.RData")
baselinedf <- readRDS("df_suppFig5_baseline_R04_testeveryone.RData")
boosteddf <- readRDS("df_suppFig5_boosted_R04_testeveryone.RData")

# or run the model
ptm <- proc.time()

phi_vec <- seq(0, 1, by = 0.05) # fine grain: by = 0.01
psi_vec <- seq(0, 1, by = 0.05)
df <- expand.grid(phi = phi_vec, psi = psi_vec)

df$Reff <- NA
df$Reff_99 <- NA
df$Reff_50 <- NA
df$totinfections_notesting <- NA
df$totinfections_99 <- NA
df$totinfections_50 <- NA

this_VE_I <- boosted_VE_I
this_VE_S <- boosted_VE_S
this_H_I  <- boosted_H_I
this_H_S  <- boosted_H_S

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
  
  df$totinfections_notesting[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                   theta = 0, q = this_q,
                                                   df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                   H_I = this_H_I, H_S = this_H_S) 
  
  df$totinfections_99[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                 theta = theta_99, q = this_q,
                                                 df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                 H_I = this_H_I, H_S = this_H_S)

  df$totinfections_50[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                 theta = theta_50, q = this_q,
                                                 df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                 H_I = this_H_I, H_S = this_H_S)
}

df$percent_reduc_inf_99 <- (df$totinfections_notesting - df$totinfections_99)/df$totinfections_notesting*100
df$percent_reduc_inf_50 <- (df$totinfections_notesting - df$totinfections_50)/df$totinfections_notesting*100

proc.time() - ptm
#saveRDS(df,file="df_suppFig5_boosted_R04_testeveryone.RData")

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
    geom_tile(aes(fill = percent_reduc_inf_50)) +
    geom_contour(aes(z = Reff_50), breaks = 1, size = 0.6, color = "white") +
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
    geom_tile(aes(fill = percent_reduc_inf_99)) +
    geom_contour(aes(z = Reff_99), breaks = 1, size = 0.6, col = "white") +
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

ggsave("suppFig5_R04_testeveryone.pdf", fig5, device = cairo_pdf, width = 8, height = 5)
ggsave("suppFig5.svg", fig5, device = svg, width = 8, height = 5)


# _____________________________________________________________________
# FIGURE 6: ####
# For omicron
# Total infections averted and percent reduction in infections for 
# baseline OMICRON scenarios assuming three testing
# scenarios (weekly 50% and 99% compliance, twice weekly 99% compliance)
#
#*
#* I: Get data ####
# _____________________________________________________________________
this_X_S <- omicron_X_S
this_X_I <- omicron_X_I
this_VE_S <- omicron_VE_S
this_VE_I <- omicron_VE_I
this_H_S <- omicron_H_S
this_H_I <- omicron_H_I

# Either read in the corresponding RDS file
df <- readRDS("df_Fig6_baseline.RData") 


# or run the model
ptm <- proc.time()

phi_vec <- seq(0, 1, by = 0.01)
psi_vec <- seq(0, 1, by = 0.01)
df <- expand.grid(phi = phi_vec, psi = psi_vec)

df$Reff <- NA
df$Reff_99 <- NA
df$Reff_50 <- NA
df$Reff_biwk <- NA
df$totinfections_notesting <- NA
df$totinfections_99 <- NA
df$totinfections_50 <- NA
df$totinfections_biwk <- NA

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
  
  df$Reff_biwk[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                theta = theta_99_biwk, q = this_q,
                                df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                H_I = this_H_I, H_S = this_H_S)
  
  df$totinfections_notesting[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                          theta = 0, q = this_q,
                                                          df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                          H_I = this_H_I, H_S = this_H_S) 
  
  df$totinfections_99[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                   theta = theta_99, q = this_q,
                                                   df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                   H_I = this_H_I, H_S = this_H_S)
  
  df$totinfections_50[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                   theta = theta_50, q = this_q,
                                                   df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                   H_I = this_H_I, H_S = this_H_S)
  
  df$totinfections_biwk[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                   theta = theta_99_biwk, q = this_q,
                                                   df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                   H_I = this_H_I, H_S = this_H_S)
}

df$percent_reduc_inf_99 <- (df$totinfections_notesting - df$totinfections_99)/df$totinfections_notesting*100
df$percent_reduc_inf_50 <- (df$totinfections_notesting - df$totinfections_50)/df$totinfections_notesting*100
df$percent_reduc_inf_biwk <- (df$totinfections_notesting - df$totinfections_biwk)/df$totinfections_notesting*100

proc.time() - ptm
#saveRDS(df,file="df_Fig6_baseline.RData")

#* II: Plot fig6 ####
#* 

df_timeseries <- data.frame(time = t)
this_phi <- 0.58
theta_vec <- c(0, theta_50, theta_99, theta_99_biwk)
for (i in 1:4){
  this_theta <- theta_vec[i]
  
  temp <- run_leaky_model(this_phi, this_VE_I, this_VE_S, this_theta, this_q,
                            psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                            H_I = this_H_I, H_S = this_H_S)
  I_tot <- temp$I_v + temp$I_u + temp$I_x + temp$I_h
  if (i == 1){
    df_timeseries$notesting <- I_tot
  } else if (i == 2){
    df_timeseries$testing50 <- I_tot
  } else if (i == 3){
    df_timeseries$testing99 <- I_tot
  } else {
    df_timeseries$testing99_biwk <- I_tot
  }
}

infections50 <- ggplot(df_timeseries, aes(x = time)) +
  geom_line(aes(y = testing50), col = theta99_purple, size = my_linesize) +
  geom_line(aes(y = notesting), col = "black", size = my_linesize) +
  ylab("Infected (#)") +
  xlab("Time (days)") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 250)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3500)) + # 2500 for R0 = 6
  alllabels_theme

infections99 <- ggplot(df_timeseries, aes(x = time)) +
  geom_line(aes(y = testing99), col = theta99_purple, size = my_linesize) +
  geom_line(aes(y = notesting), col = "black", size = my_linesize) +
  ylab("Infected (#)") +
  xlab("Time (days)") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 250)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3500)) + # 2500 for R0 = 6
  alllabels_theme

infections99_biwk <- ggplot(df_timeseries, aes(x = time)) +
  geom_line(aes(y = testing99_biwk), col = theta99_purple, size = my_linesize) +
  geom_line(aes(y = notesting), col = "black", size = my_linesize) +
  ylab("Infected (#)") +
  xlab("Time (days)") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 250)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3500)) + # 2500 for R0 = 6
  alllabels_theme

percentreduc50 <- ggplot(df, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) +
  geom_tile(aes(fill = percent_reduc_inf_50)) +
  geom_contour(aes(z = Reff_50), breaks = 1, size = 0.6, color = "white") +
  geom_contour(aes(z = Reff), breaks = 1, size = 0.6, col = "white", linetype = "longdash") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_point(aes(x = 58, y = 35), color = "white") +
  ylab("") +#"Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") +
  ggtitle("") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) +
  coord_fixed(1) +
  theme(legend.position = "none")

percentreduc99 <- ggplot(df, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) +
  geom_tile(aes(fill = percent_reduc_inf_99)) +
  geom_contour(aes(z = Reff_99), breaks = 1, size = 0.6, col = "white") +
  geom_contour(aes(z = Reff), breaks = 1, size = 0.6, col = "white", linetype = "longdash") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_point(aes(x = 58, y = 35), color = "white") +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") +
  ggtitle("Weekly testing, 99% compliance", "% reduction in infections due to testing") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) +
  coord_fixed(1) +
  labs(fill = "") +
  theme(axis.title.y = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank()) #element_text(hjust = 0.5))

percentreducbiwk <- ggplot(df, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) +
  geom_tile(aes(fill = percent_reduc_inf_biwk)) +
  geom_contour(aes(z = Reff_biwk), breaks = 1, size = 0.6, col = "white") +
  geom_contour(aes(z = Reff), breaks = 1, size = 0.6, col = "white", linetype = "longdash") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_point(aes(x = 58, y = 35), color = "white") +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") +
  ggtitle("Biweekly testing, 99% compliance", "% reduction in infections due to testing") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) +
  coord_fixed(1) +
  labs(fill = "") +
  theme(axis.title.y = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank()) #element_text(hjust = 0.5))

percent_legend <- get_legend(percentreducbiwk)

percentreduc50_baseline <- percentreduc50 + alllabels_theme + ggtitle("Weekly, 50% Compliance")
percentreduc99_baseline <- percentreduc99 + onlyx_theme + 
  theme(legend.position = "none") + ggtitle("Weekly, 99% Compliance") +
  xlab("Population vaccination rate (%)")
percentreducbiwk_baseline <- percentreducbiwk + onlyx_theme + ggtitle("Biweekly, 99% Compliance")

# panels <- ggarrange(percentreduc50_baseline, NULL,
#                     percentreduc99_baseline, NULL, 
#                     percentreducbiwk_baseline,
#                     nrow = 1, ncol = 5,
#                     align = "hv",
#                     widths = c(1, -0.05, 1, -0.05, 1),
#                     labels = c(" a", NA, "   b", NA,"  c"),
#                     label.y = 0.7)

panels <- ggarrange(infections50, NULL,
                    infections99, NULL,
                    infections99_biwk,
                    percentreduc50_baseline, NULL,
                    percentreduc99_baseline, NULL, 
                    percentreducbiwk_baseline,
                    nrow = 2, ncol = 5,
                    align = "hv",
                    widths = c(1, -0.05, 1, -0.05, 1),
                    heights = c(0.4, 1))
                    #labels = c(" a", NA, "   b", NA,"  c"),
                    #label.y = 0.7)

lay <- rbind(c(1, 2))

fig6 <- arrangeGrob(panels, percent_legend, layout_matrix = lay,
                    widths = c(3, 0.5), 
                    left = c("Infection-acquired immunity (%)"))

ggsave("exampleFig6.pdf", fig6, device = cairo_pdf, width = 8, height = 5)
ggsave("suppFig6.svg", fig6, device = svg, width = 8, height = 5)
