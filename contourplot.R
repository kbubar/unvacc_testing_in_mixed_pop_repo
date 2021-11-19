# contourplot.R - generates heatmaps (main text and supp)
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
df <- readRDS("df_fig2_setupparams.RData")
#df <- readRDS("df_suppfig2_R06.RData")

# or run the model 
ptm <- proc.time()
phi_vec <- seq(0, 1, by = 0.05) # fine grid : by = 0.01
psi_vec <- seq(0, 1, by = 0.05)
df <- expand.grid(phi = phi_vec, psi = psi_vec)

df$Reff <- NA
df$breakthrough <- NA
df$dom_transmission <- NA
df$tot_infections <- NA

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

#saveRDS(df,file="df_fig2_setupparams.RData")

proc.time() - ptm

# _____________________________________________________________________
#* II: Plot fig2 ####
# _____________________________________________________________________

# plot Reff
p_Reff <- ggplot(df, aes(x = phi*100, y = psi*100, z = Reff))+ #, colour = ..level..)) + 
  geom_tile(aes(fill = tot_infections)) +
  geom_contour(breaks = 1:R0, size = 0.4, color = "white") +
  # geom_contour2(aes(label = ..level..),breaks = 1:R0, size = 0.4, color = "white",
  #               skip = 0) +
  geom_text_contour(breaks = 1:R0, color = "white", rotate = FALSE,
                     nudge_y = 1,
                   nudge_x = 2)+
  #stat_contour(breaks = 1:R0, size = 1, color = "white") + 
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

# FIXME - labeling for R
#p_Reff <- direct.label(p, list("smart.grid"))

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

ggarrange(p_Reff, p_infection,
          p_transmission,
          widths = c(1.26, 1, 1.23),
          labels = c('a  ', 'b', 'c'),
          ncol = 3,
          label.y = 0.92,
          align = "hv")

ggsave("fig2_heatmap.pdf", device = cairo_pdf, width = 12, height = 4)
ggsave("fig2_heatmap.svg", device = svg, width = 12, height = 4)

# To find transition points:
infection_transitions <- df[df$breakthrough >= 50,]  %>% group_by(psi) %>% summarize(phi=min(phi))
transmission_transitions <- df[df$dom_transmission < 50,]  %>% group_by(psi) %>% summarize(phi=min(phi))
print(paste0("Infection transition point: ",min(infection_transitions$phi), " - ",max(infection_transitions$phi)))
print(paste0("Transmission transition point: ",min(transmission_transitions$phi), " - ",max(transmission_transitions$phi)))


# _____________________________________________________________________
# FIGURE 3: ####
# Reff and total infections for the three scenarios
#
#*
#* I: Get data ####
# _____________________________________________________________________

# Either read in the corresponding RDS file

# or run the model 
ptm <- proc.time()
phi_vec <- seq(0, 1, by = 0.05) # fine grid : by = 0.01
psi_vec <- seq(0, 1, by = 0.05)
df <- expand.grid(phi = phi_vec, psi = psi_vec)

df$Reff_baseline <- NA
df$tot_infections_baseline <- NA
df$Reff_waning <- NA
df$tot_infections_waning <- NA
df$Reff_boosting <- NA
df$tot_infections_boosting <- NA

for (this_scenario in 1:3){
  if (this_scenario == 1){
    # waning
    my_VE_I <- low_VE_I
    my_VE_S <- low_VE_S
    my_H_I <- low_H_I
    my_H_S <- low_H_S
  } else if (this_scenario == 2){
    # baseline
    my_VE_I <- this_VE_I
    my_VE_S <- this_VE_S
    my_H_I <- this_H_I
    my_H_S <- this_H_S
  } else {
    # boosting
    my_VE_I <- boosted_VE_I
    my_VE_S <- boosted_VE_S
    my_H_I <- boosted_H_I
    my_H_S <- boosted_H_S
  }
  
  tot_infections <- c()
  Reff <- c()
  for (i in 1:dim(df)[1]){
    tot_infections[i] <- compute_tot_infections(df$phi[i], VE_I = my_VE_I, VE_S = my_VE_S,  
                                                   theta = 0, q = this_q, 
                                                   df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                   H_I = my_H_I, H_S = my_H_S)
    Reff[i] <- compute_Reff(df$phi[i], VE_I = my_VE_I, VE_S = my_VE_S,
                               theta = 0, q = this_q,
                               df$psi[i], X_I = this_X_I, X_S = this_X_S,
                               H_I = my_H_I, H_S = my_H_S)
  }
  if (this_scenario == 1){
    df$tot_infections_waning <- tot_infections
    df$Reff_waning <- Reff
  } else if (this_scenario == 2){
    df$tot_infections_baseline <- tot_infections
    df$Reff_baseline <- Reff
  } else {
    df$tot_infections_boosted <- tot_infections
    df$Reff_boosted <- Reff
  }
}
#saveRDS(df,file="df_fig2_setupparams.RData")

proc.time() - ptm

# _____________________________________________________________________
#* II: Plot fig3 ####
# _____________________________________________________________________

p_Reff_waning <- ggplot(df, aes(x = phi*100, y = psi*100, z = Reff_waning))+ #, colour = ..level..)) + 
  geom_tile(aes(fill = tot_infections_waning)) +
  geom_contour(breaks = 1:R0, size = 0.4, color = "white") +
  geom_text_contour(breaks = 1:R0, color = "white", rotate = FALSE, nudge_y = 1, nudge_x = 2, skip = 0)+
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") + 
  ggtitle("Waning scenario") +
  scale_fill_viridis(option="viridis", limits = c(0, N)) +
  coord_fixed(1) + 
  theme(legend.position = "none") 

p_Reff_baseline <- ggplot(df, aes(x = phi*100, y = psi*100, z = Reff_baseline))+ #, colour = ..level..)) + 
  geom_tile(aes(fill = tot_infections_baseline)) +
  geom_contour(breaks = 1:R0, size = 0.4, color = "white") +
  geom_text_contour(breaks = 1:R0, color = "white", rotate = FALSE, nudge_y = 1, nudge_x = 2, skip = 0)+
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("") +
  xlab("Population vaccination rate (%)") + 
  ggtitle("Baseline") +
  scale_fill_viridis(option="viridis", limits = c(0, N)) +
  coord_fixed(1) + 
  theme(legend.position = "none", 
        axis.title.y = element_blank()) 

p_Reff_boosted <- ggplot(df, aes(x = phi*100, y = psi*100, z = Reff_boosted))+ #, colour = ..level..)) + 
  geom_tile(aes(fill = tot_infections_boosted)) +
  geom_contour(breaks = 1:R0, size = 0.4, color = "white") +
  geom_text_contour(breaks = 1:R0, color = "white", rotate = FALSE, nudge_y = 1, nudge_x = 2, skip = 0)+
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("") +
  xlab("") + # xlab("Population vaccination rate (%)") + 
  ggtitle("Boosted scenario") +
  scale_fill_viridis(option="viridis", limits = c(0, N)) +
  coord_fixed(1) + 
  labs(fill = "") +
  theme(legend.text = element_text(size = 11), 
        legend.spacing.x = unit(0.7, 'cm'),
        axis.title.y = element_blank()) 

num_legend <- get_legend(p_Reff_boosted)

p_Reff_boosted <- p_Reff_boosted + theme(legend.position = "none")

panels <- ggarrange(p_Reff_waning, p_Reff_baseline, p_Reff_boosted,
          widths = c(1, 1, 1),
          labels = c('a  ', 'b', 'c'),
          ncol = 3,
          label.y = 0.92,
          align = "hv")

fig3 <- arrangeGrob(panels, num_legend,
             nrow = 1,
             widths = c(3, 0.5))

ggsave("Fig3.pdf", fig3, device = cairo_pdf, width = 12, height = 4)
ggsave("Fig3.svg", device = svg, width = 12, height = 4)

# _____________________________________________________________________
# FIGURE 4: ####
# Total infections averted and percent reduction in infections for 
# all three VE scenarios (waning, baseline, boosted) and
# both testing scenarios (weekly, 50% and 99% compliance)
#
#*
#* I: Get data ####
# _____________________________________________________________________

# Either read in the corresponding RDS file

# or run the model
ptm <- proc.time()

phi_vec <- seq(0, 1, by = 0.05)
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

# stat_contour(aes(z = Reff_99), breaks = 1, size = mylinesize, col = "white") + 
  #   #stat_contour(aes(z = Reff), breaks = 1, size = mylinesize, col = mylightgray, linetype = "longdash") + 
  
p_totinfections_50 <- ggplot(df, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) + 
  geom_tile(aes(fill = totinfections_50)) +
  geom_contour(aes(z = Reff_50), breaks = 1, size = 0.4, color = "white") +
  geom_contour(aes(z = Reff), breaks = 1, size = 0.4, col = mylightgray, linetype = "longdash") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") + 
  ggtitle("Weekly testing, 50% compliance", "Total infections") +
  scale_fill_viridis(option="viridis", limits = c(0, N)) +
  coord_fixed(1) + 
  theme(legend.position = "none",
        plot.title = element_text(color = myyellow),
        plot.subtitle = element_text(hjust = 0.5)) 

p_totinfections_99 <- ggplot(df, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) + 
  geom_tile(aes(fill = totinfections_99)) +
  geom_contour(aes(z = Reff_99), breaks = 1, size = 0.4, color = "white") +
  geom_contour(aes(z = Reff), breaks = 1, size = 0.4, col = mylightgray, linetype = "longdash") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") + 
  ggtitle("Weekly testing, 99% compliance", "Total infections") +
  scale_fill_viridis(option="viridis", limits = c(0, N)) +
  coord_fixed(1) + 
  theme(legend.position = "none",
        plot.title = element_text(color = myblue),
        plot.subtitle = element_text(hjust = 0.5)) 

p_percentreduc_50 <- ggplot(df, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) + 
  geom_tile(aes(fill = percent_reduc_inf_50)) +
  geom_contour(aes(z = Reff_50), breaks = 1, size = 0.4, color = "white") +
  geom_contour(aes(z = Reff), breaks = 1, size = 0.4, col = mylightgray, linetype = "longdash") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") + 
  ggtitle("Weekly testing, 50% compliance", "% reduction in infections due to testing") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) + 
  coord_fixed(1) + 
  theme(legend.position = "none",
        plot.title = element_text(color = myyellow),
        plot.subtitle = element_text(hjust = 0.5)) 

p_percentreduc_99 <- ggplot(df, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) + 
  geom_tile(aes(fill = percent_reduc_inf_99)) +
  geom_contour(aes(z = Reff_99), breaks = 1, size = 0.4, color = "white") +
  geom_contour(aes(z = Reff), breaks = 1, size = 0.4, col = mylightgray, linetype = "longdash") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") + 
  ggtitle("Weekly testing, 99% compliance", "% reduction in infections due to testing") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) + 
  coord_fixed(1) + 
  labs(fill = "") +
  theme(plot.title = element_text(color = myblue),
        plot.subtitle = element_text(hjust = 0.5)) 

percent_legend <- get_legend(p_percentreduc_99)

p_percentreduc_99 <- p_percentreduc_99 + theme(legend.position = "none")

panels <- ggarrange(p_totinfections_50, p_percentreduc_50,
          p_totinfections_99, p_percentreduc_99,
          nrow = 2, ncol = 2)

lay <- rbind(c(1, 2),
             c(1, 3))

grid.arrange(panels, num_legend, percent_legend,
             layout_matrix = lay)

fig4 <- arrangeGrob(panels, num_legend, percent_legend,
                    layout_matrix = lay,
                    widths = c(3, 0.5))

ggsave("Fig4_boosted.pdf", fig4, device = cairo_pdf, width = 8, height = 8)
# # _____________________________________________________________________
# # FIGURE 4: ####
# # Infections averted and infections averted/100 tests over phi and psi
# # Moderate testing: weekly, 99% compliance
# # Realistic testing: weekly, 50% compliance
# #
# #*
# #* I: Get data ####
# # _____________________________________________________________________
# 
# # Either read in the corresponding RDS file
# df <- readRDS("df_fig4_setupparams.RData")
# #df <- readRDS("df_suppfig4_R06.RData")
# 
# # or run the model 
# ptm <- proc.time()
# 
# phi_vec <- seq(0, 1, by = 0.05)
# psi_vec <- seq(0, 1, by = 0.05)
# df <- expand.grid(phi = phi_vec, psi = psi_vec)
# 
# df$Reff <- NA
# df$Reff_99 <- NA
# df$Reff_50 <- NA
# df$totinfections_99 <- NA
# df$totinfections_50 <- NA
# df$infections_averted_99 <- NA
# df$infections_averted_50 <- NA
# df$infections_averted_per100_99 <- NA
# df$infections_averted_per100_50 <- NA
# 
# for (i in 1:dim(df)[1]){
#   df$Reff[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,  
#                              theta = 0, q = this_q, 
#                              df$psi[i], X_I = this_X_I, X_S = this_X_S,
#                              H_I = this_H_I, H_S = this_H_S)
# 
#   df$Reff_99[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
#                                  theta = theta_99, q = this_q, 
#                                  df$psi[i], X_I = this_X_I, X_S = this_X_S,
#                                  H_I = this_H_I, H_S = this_H_S)
#   df$Reff_50[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
#                                   theta = theta_50, q = this_q, 
#                                   df$psi[i], X_I = this_X_I, X_S = this_X_S,
#                                   H_I = this_H_I, H_S = this_H_S)
#   
#   df$totinfections_notesting[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
#                                     theta = 0, q = this_q, 
#                                     df$psi[i], X_I = this_X_I, X_S = this_X_S,
#                                     H_I = this_H_I, H_S = this_H_S)
#   
#   df$totinfections_99[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
#                                                  theta = theta_99, q = this_q, 
#                                                  df$psi[i], X_I = this_X_I, X_S = this_X_S,
#                                                  H_I = this_H_I, H_S = this_H_S)
#   
#   df$totinfections_50[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
#                                                  theta = theta_50, q = this_q, 
#                                                  df$psi[i], X_I = this_X_I, X_S = this_X_S,
#                                                  H_I = this_H_I, H_S = this_H_S)
#   
#   df$infections_averted_99[i] <- df$totinfections_notesting[i] - df$totinfections_99[i]
#   
#   df$infections_averted_50[i] <- df$totinfections_notesting[i] - df$totinfections_50[i]
#   
#   num_tests_99 <- compute_num_tests(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
#                                      theta = theta_99, freq = low_freq, inf_period = 1/gamma,
#                                      compliance = high_compliance, q = this_q, 
#                                      df$psi[i], this_X_I, this_X_S, this_H_I, this_H_S)
#   
#   num_tests_50 <- compute_num_tests(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
#                                       theta = theta_50, freq = low_freq, inf_period = 1/gamma,
#                                       compliance = low_compliance, q = this_q, 
#                                       df$psi[i], this_X_I, this_X_S, this_H_I, this_H_S)
#   
#   df$infections_averted_per100_99[i] <- df$infections_averted_99[i]/num_tests_99*100
#   df$infections_averted_per100_50[i] <- df$infections_averted_50[i]/num_tests_50*100
# }
# 
# proc.time() - ptm
# #df[is.na(df)] <- 0 # fix NAs (from dividing by 0 when there are no tests administered)
# 
# 
# #saveRDS(df,file="df_fig4_setupparams.RData")
# 
# # _____________________________________________________________________
# #* II: Plot fig4 ####
# # _____________________________________________________________________
# mylinesize <- 0.4
# 
# infectavert <- ggplot(df, aes(x = phi*100, y = psi*100, fill = totinfections_notesting)) + 
#   geom_tile() +
#   stat_contour(aes(z = Reff), breaks = 1, size = mylinesize, col = mylightgray, linetype = "longdash") + 
#   scale_y_continuous(expand = c(0,0)) +
#   scale_x_continuous(expand = c(0,0)) +
#   ylab("") + #ylab("Infection-acquired immunity (%)") +
#   xlab("") + #xlab("Population vaccination rate (%)") + 
#   ggtitle("No testing", "")+#"Infections averted\nrel. to no testing") + 
#   labs(fill = "") + 
#   scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,20000)) + #14000 for R0=6
#   theme(legend.text = element_text(size = 11), 
#         legend.spacing.x = unit(0.75, 'cm'),
#         plot.title = element_text(color = mygray),
#         plot.subtitle = element_text(hjust = 0.5),
#         axis.text.y = element_blank()) + 
#   coord_fixed(1)
# 
# infectavert_99 <- ggplot(df, aes(x = phi*100, y = psi*100, fill = totinfections_99)) + 
#   geom_tile() +
#   stat_contour(aes(z = Reff_99), breaks = 1, size = mylinesize, col = "white") + 
#   #stat_contour(aes(z = Reff), breaks = 1, size = mylinesize, col = mylightgray, linetype = "longdash") + 
#   scale_y_continuous(expand = c(0,0)) +
#   scale_x_continuous(expand = c(0,0)) +
#   ylab("") + #ylab("Infection-acquired immunity (%)") +
#   xlab("") + #xlab("Population vaccination rate (%)") + 
#   ggtitle("Weekly testing, 99% compliance", "")+#"Infections averted\nrel. to no testing") + 
#   labs(fill = "") + 
#   scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,20000)) + #14000 for R0=6
#   theme(legend.text = element_text(size = 11), 
#         legend.spacing.x = unit(0.75, 'cm'),
#         plot.title = element_text(color = myblue),
#         plot.subtitle = element_text(hjust = 0.5),
#         axis.text.y = element_blank()) + 
#   coord_fixed(1)
# 
# infectavert_50 <- ggplot(df, aes(x = phi*100, y = psi*100, fill = totinfections_50)) + 
#   geom_tile() +
#   stat_contour(aes(z = Reff_50), breaks = 1, size = mylinesize, col =  "white") + 
#   #stat_contour(aes(z = Reff), breaks = 1, size = mylinesize, col = mylightgray, linetype = "longdash") + 
#   scale_y_continuous(expand = c(0,0)) +
#   scale_x_continuous(expand = c(0,0)) +
#   ylab("") + #ylab("Infection-acquired immunity (%)") +
#   xlab("") + #xlab("Population vaccination rate (%)") + 
#   ggtitle("Weekly testing, 50% compliance", "") +# "Infections averted\nrel. to no testing") + 
#   labs(fill = "") + 
#   scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,20000)) + #14000 for R0=6
#   theme(#legend.position = "none",
#         plot.title = element_text(color = myyellow),
#         plot.subtitle = element_text(hjust = 0.5)) + 
#   coord_fixed(1)
# 
# infper100_99 <- ggplot(df, aes(x = phi*100, y = psi*100, fill = infections_averted_per100_99)) + 
#   geom_tile() +
#   stat_contour(aes(z = Reff_99), breaks = 1, size = mylinesize, col =  "white") + 
#   stat_contour(aes(z = Reff), breaks = 1, size = mylinesize, col = mylightgray, linetype = "longdash") + 
#   scale_y_continuous(expand = c(0,0)) +
#   scale_x_continuous(expand = c(0,0)) +
#   ylab("") + #ylab("Infection-acquired immunity (%)") +
#   xlab("") + #xlab("Population vaccination rate (%)") + 
#   ggtitle("") + #subtitle = "Infections averted per 100 tests") + 
#   labs(fill = "") + 
#   scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,8)) + # 8 for R0 = 6
#   theme(legend.text = element_text(size = 11), 
#         legend.spacing.x = unit(0.75, 'cm'),
#         legend.text.align = 0,
#         plot.subtitle = element_text(hjust = 0.5),
#         axis.text.y = element_blank()) + 
#   coord_fixed(1)
# 
# 
# infper100_50 <- ggplot(df, aes(x = phi*100, y = psi*100, fill = infections_averted_per100_50)) + 
#   geom_tile() +
#   stat_contour(aes(z = Reff_50), breaks = 1, size = mylinesize, col = "white") + 
#   stat_contour(aes(z = Reff), breaks = 1, size = mylinesize, col = mylightgray, linetype = "longdash") + 
#   scale_y_continuous(expand = c(0,0)) +
#   scale_x_continuous(expand = c(0,0)) +
#   ylab("") + #ylab("Infection-acquired immunity (%)") +
#   xlab("") + #xlab("Population vaccination rate (%)") + 
#   ggtitle("")+ #subtitle = "Infections averted per 100 tests") + 
#   labs(fill = "") + 
#   scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,8)) + 
#   theme(legend.position = "none",
#         plot.subtitle = element_text(hjust = 0.5)) + 
#   coord_fixed(1)
# 
# fig4 <- ggarrange(infectavert_50, NULL, infectavert_99, NULL,
#                   NULL,NULL,NULL, NULL,
#                   infper100_50, NULL, infper100_99, NULL,
#           ncol = 4, nrow = 3,
#           widths = c(1, -0.05, 1.178, 0.05),
#           heights = c(1, -0.11, 1),
#           align = "hv",
#           labels = c("  a", NA, "  b", NA, NA, NA, NA, NA, "  c", NA, "  d", NA),
#           label.y = 0.87)
# 
# annotate_figure(fig4,
#                 bottom = text_grob("Population vaccination rate (%)", size = 14, family = "Arial",
#                                    vjust = -1.2),
#                 left = text_grob("Infection-acquired immunity (%)", size = 14, family = "Arial", rot = 90,
#                                  vjust = 1.1))
# 
# ggsave("suppfig4_heatmap_R06.pdf", device = cairo_pdf, width = 8, height = 7.5)
# ggsave("suppfig4_heatmap_R06.svg", device = svg, width = 8, height = 7.5)
# 
