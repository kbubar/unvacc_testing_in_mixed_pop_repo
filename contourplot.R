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
  geom_text_contour(breaks = 1:R0, color = "white", rotate = FALSE,
                    nudge_y = 1,
                    nudge_x = 4)+
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

ggarrange(p_Reff, p_infection,
          p_transmission,
          widths = c(1.26, 1, 1.23),
          labels = c('a  ', 'b', 'c'),
          ncol = 3,
          label.y = 0.92,
          align = "hv")

ggsave("SuppFig2.pdf", device = cairo_pdf, width = 12, height = 4)
ggsave("Fig2.svg", device = svg, width = 12, height = 4)

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

ggsave("SuppFig3.pdf", fig3, device = cairo_pdf, width = 12, height = 4)
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
df <- readRDS("df_fig4_setupparams.RData")

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

this_VE_I <- low_VE_I
this_VE_S <- low_VE_S
this_H_I  <- low_H_I
this_H_S  <- low_H_S

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

ggsave("SuppFig4_waning.pdf", fig4, device = cairo_pdf, width = 8, height = 8)


# _____________________________________________________________________
# FIGURE 5: ####
# Transition points
# _____________________________________________________________________

phi_vec <- seq(0.5, 1, by = 0.01)
psi_vec <- seq(0, 1, by = 0.01)

for (i in 1){

baseline_VE_I = this_VE_I
baseline_H_I = this_H_I
baseline_VE_S = this_VE_S
baseline_H_S = this_H_S
baseline_df <- expand.grid(phi = phi_vec, psi = psi_vec)
baseline_df$breakthroughs_notesting <- NA
baseline_df$breakthroughs_99 <- NA
baseline_df$breakthroughs_50 <- NA
baseline_df$v_transmission_notesting <- NA
baseline_df$v_transmission_99 <- NA
baseline_df$v_transmission_50 <- NA
baseline_df$VE <- "baseline"

boosted_df <- expand.grid(phi = phi_vec, psi = psi_vec)
boosted_df$breakthroughs_notesting <- NA
boosted_df$breakthroughs_99 <- NA
boosted_df$breakthroughs_50 <- NA
boosted_df$v_transmission_notesting <- NA
boosted_df$v_transmission_99 <- NA
boosted_df$v_transmission_50 <- NA
boosted_df$VE <- "boosted"

lowVE_df <- expand.grid(phi = phi_vec, psi = psi_vec)
lowVE_df$breakthroughs_notesting <- NA
lowVE_df$breakthroughs_99 <- NA
lowVE_df$breakthroughs_50 <- NA
lowVE_df$v_transmission_notesting <- NA
lowVE_df$v_transmission_99 <- NA
lowVE_df$v_transmission_50 <- NA
lowVE_df$VE <- "low"

for (i in 1:dim(baseline_df)[1]){
  
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
  
  # Low VE scenario 
  lowVE_df$breakthroughs_notesting[i] <- compute_percent_breakthrough_infections(lowVE_df$phi[i], 
                                                         VE_I = low_VE_I, VE_S = low_VE_S,
                                                         theta = 0, q = this_q,
                                                         lowVE_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                         H_I = low_H_I, H_S = low_H_S) 
  lowVE_df$breakthroughs_99[i] <- compute_percent_breakthrough_infections(lowVE_df$phi[i], 
                                                          VE_I = low_VE_I, VE_S = low_VE_S,
                                                          theta = theta_99, q = this_q,
                                                          lowVE_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                          H_I = low_H_I, H_S = low_H_S)
  lowVE_df$breakthroughs_50[i] <- compute_percent_breakthrough_infections(lowVE_df$phi[i], 
                                                          VE_I = low_VE_I, VE_S = low_VE_S,
                                                          theta = theta_50, q = this_q,
                                                          lowVE_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                          H_I = low_H_I, H_S = low_H_S)
  
  lowVE_df$v_transmission_notesting[i] <- 100 - compute_dominant_transmission(lowVE_df$phi[i], 
                                                          VE_I = low_VE_I, VE_S = low_VE_S,
                                                          theta = 0, q = this_q,
                                                          lowVE_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                          H_I = low_H_I, H_S = low_H_S) * 100
  lowVE_df$v_transmission_99[i] <- 100 - compute_dominant_transmission(lowVE_df$phi[i], 
                                                         VE_I = low_VE_I, VE_S = low_VE_S,
                                                         theta = theta_99, q = this_q,
                                                         lowVE_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                         H_I = low_H_I, H_S = low_H_S) * 100
  lowVE_df$v_transmission_50[i] <- 100 - compute_dominant_transmission(lowVE_df$phi[i], 
                                                         VE_I = low_VE_I, VE_S = low_VE_S,
                                                         theta = theta_50, q = this_q,
                                                         lowVE_df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                         H_I = low_H_I, H_S = low_H_S) * 100
}

# compute summary statistics
VEs <- c(rep(baseline_VE_S,3),rep(boosted_VE_S,3),rep(low_VE_S,3))
df <- data.frame(VE = VEs)
df$testing <- rep(c(0,50,99),3)
df$max_inf_transition <- NA
df$min_inf_transition <- NA
df$max_trnsmsn_transition <- NA
df$min_trnsmsn_transition <- NA

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

# Low VE
lowVE_inf_transitions_notesting <- lowVE_df[lowVE_df$breakthroughs_notesting >= 50,] %>%
  group_by(psi) %>% summarize(phi=min(phi))
df[df$testing==0 & df$VE==low_VE_S,]$min_inf_transition <- min(lowVE_inf_transitions_notesting$phi)
df[df$testing==0 & df$VE==low_VE_S,]$max_inf_transition <-  max(lowVE_inf_transitions_notesting$phi)
lowVE_inf_transitions_99 <- lowVE_df[lowVE_df$breakthroughs_99 >= 50,] %>%
  group_by(psi) %>% summarize(phi=min(phi))
df[df$testing==99 & df$VE==low_VE_S,]$min_inf_transition <- min(lowVE_inf_transitions_99$phi)
df[df$testing==99 & df$VE==low_VE_S,]$max_inf_transition <- max(lowVE_inf_transitions_99$phi)
lowVE_inf_transitions_50 <- lowVE_df[lowVE_df$breakthroughs_50 >= 50,] %>%
  group_by(psi) %>% summarize(phi=min(phi))
df[df$testing==50 & df$VE==low_VE_S,]$min_inf_transition <- min(lowVE_inf_transitions_50$phi)
df[df$testing==50 & df$VE==low_VE_S,]$max_inf_transition <- max(lowVE_inf_transitions_50$phi)

lowVE_trnsmsn_transitions_notesting <- lowVE_df[lowVE_df$v_transmission_notesting >= 50,] %>%
  group_by(psi) %>% summarize(phi=min(phi))
df[df$testing==0 & df$VE==low_VE_S,]$min_trnsmsn_transition <- min(lowVE_trnsmsn_transitions_notesting$phi)
df[df$testing==0 & df$VE==low_VE_S,]$max_trnsmsn_transition <- max(lowVE_trnsmsn_transitions_notesting$phi)
lowVE_trnsmsn_transitions_99 <- lowVE_df[lowVE_df$v_transmission_99 >= 50,] %>%
  group_by(psi) %>% summarize(phi=min(phi))
df[df$testing==99 & df$VE==low_VE_S,]$min_trnsmsn_transition <- min(lowVE_trnsmsn_transitions_99$phi)
df[df$testing==99 & df$VE==low_VE_S,]$max_trnsmsn_transition <- max(lowVE_trnsmsn_transitions_99$phi)
lowVE_trnsmsn_transitions_50 <- lowVE_df[lowVE_df$v_transmission_50 >= 50,] %>%
  group_by(psi) %>% summarize(phi=min(phi))
df[df$testing==50 & df$VE==low_VE_S,]$min_trnsmsn_transition <- min(lowVE_trnsmsn_transitions_50$phi)
df[df$testing==50 & df$VE==low_VE_S,]$max_trnsmsn_transition <- max(lowVE_trnsmsn_transitions_50$phi)

}

# Plot Figure 5
p_inf_transition <- ggplot() + 
  geom_errorbar(data = df[df$testing == 0,],
    aes(VE, ymin = min_inf_transition, ymax = max_inf_transition), col = mylightgray,
    width = 0.025, position = position_nudge(x = -0.05) # no testing 
  ) +
  geom_errorbar(data = df[df$testing == 50,],
                aes(VE, ymin = min_inf_transition, ymax = max_inf_transition), col = mypurple,
                width = 0.025                         # 50% compliance testing 
  ) +
  geom_errorbar(data = df[df$testing == 99,],
                aes(VE, ymin = min_inf_transition, ymax = max_inf_transition), col = mygray,
                width = 0.025, position = position_nudge(x = 0.05) # 99% compliance testing 
  ) +
   ylim(0.5, 1) +
   xlab("Vaccine Effectiveness") +
   ylab("Proportion Vaccinated (%)") +
   ggtitle("Infection Transition Point") +
   theme(legend.position = "none")
   
p_trnsmsn_transition <- ggplot() + 
  geom_errorbar(data = df[df$testing == 0,],
                aes(VE, ymin = min_trnsmsn_transition, ymax = max_trnsmsn_transition), col = mylightgray,
                width = 0.025, position = position_nudge(x = -0.05) # no testing 
  ) +
  geom_errorbar(data = df[df$testing == 50,],
                aes(VE, ymin = min_trnsmsn_transition, ymax = max_trnsmsn_transition), col = mypurple,
                width = 0.025                         # 50% compliance testing 
  ) +
  geom_errorbar(data = df[df$testing == 99,],
                aes(VE, ymin = min_trnsmsn_transition, ymax = max_trnsmsn_transition), col = mygray,
                width = 0.025, position = position_nudge(x = 0.05) # 99% compliance testing 
  ) +
 ylim(0.5, 1) +
 xlab("Vaccine Effectiveness") +
 #ylab("Vacc. Rate at Which Vacc. Dominate Transmission") +
 ggtitle("Transmission Transition Point") +
 theme()   
   
fig5 <- ggarrange(p_inf_transition, p_trnsmsn_transition,
                    widths = c(1, 1),
                    labels = c('a  ', 'b'),
                    ncol = 2,
                    label.y = 0.92,
                    align = "hv")
fig5

ggsave("Fig5.pdf", fig5, device = cairo_pdf, width = 8, height = 4)


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
