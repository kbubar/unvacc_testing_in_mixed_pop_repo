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
df <- readRDS("df_suppfig2_R06.RData")

# or run the model 
ptm <- proc.time()
phi_vec <- seq(0, 1, by = 0.01) # fine grid : by = 0.01
psi_vec <- seq(0, 1, by = 0.01)
df <- expand.grid(phi = phi_vec, psi = psi_vec)

df$Reff <- NA
df$breakthrough <- NA
df$dom_transmission <- NA

for (i in 1:dim(df)[1]){
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
p <- ggplot(df, aes(x = phi*100, y = psi*100, z = Reff, colour = ..level..)) + 
  stat_contour(breaks = 1:R0, size = 1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") + 
  ggtitle(expression(R[eff])) 

p_Reff <- direct.label(p, list("smart.grid"))

# plot % infections in the unvaccinated
p_breakthrough <- ggplot(df, aes(x = phi*100, y = psi*100, z = breakthrough)) + 
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
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno")) 
  # annotate("text", label = "50",
  #           x = 65, y = 85, colour = "white")  

# plot dominant transmission
p_dom <- ggplot(df, aes(x = phi*100, y = psi*100, z = dom_transmission)) + 
  geom_tile(aes(fill = dom_transmission)) +
  stat_contour(breaks = c(50), size = 0.4, color = "white") + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + # ylab("Infection-acquired immunity (%)") +
  xlab("") + # xlab("Population vaccination rate (%)") + 
  ggtitle("% of transmission from\nunvaccinated individuals") + 
 labs(fill = "")+#"Percent of\ntransmission\nby unvacc.") +
  theme(legend.text = element_text(size = 11), 
        legend.title = element_text(size = 11),
        legend.title.align = 0.5,
        legend.spacing.x = unit(0.5, 'cm'),
        axis.text.y = element_blank()) +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno")) #+ 

ggarrange(p_Reff, p_breakthrough, p_dom,
          ncol = 3,
          widths = c(1, 1.15, 1.3),
          align = "hv")

ggsave("suppfig2_heatmap_R06.pdf", device = cairo_pdf, width = 12, height = 4)
ggsave("suppfig2_heatmap_R06.svg", device = svg, width = 12, height = 4)


# _____________________________________________________________________
# FIGURE 4: ####
# Infections averted and infections averted/100 tests over phi and psi
# Moderate testing: weekly, 99% compliance
# Realistic testing: weekly, 50% compliance
#
#*
#* I: Get data ####
# _____________________________________________________________________

# Either read in the corresponding RDS file
df <- readRDS("df_fig4_setupparams.RData")
df <- readRDS("df_suppfig4_R06.RData")

# or run the model 
ptm <- proc.time()

phi_vec <- seq(0, 1, by = 0.01)
psi_vec <- seq(0, 1, by = 0.01)
df <- expand.grid(phi = phi_vec, psi = psi_vec)

df$Reff <- NA
df$Reff_mod <- NA
df$Reff_real <- NA
df$infections_averted_mod <- NA
df$infections_averted_real <- NA
df$infections_averted_per100_mod <- NA
df$infections_averted_per100_real <- NA

for (i in 1:dim(df)[1]){
  df$Reff[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,  
                             theta = 0, q = this_q, 
                             df$psi[i], X_I = this_X_I, X_S = this_X_S,
                             H_I = this_H_I, H_S = this_H_S)

  df$Reff_mod[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
                                 theta = mod_theta, q = this_q, 
                                 df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                 H_I = this_H_I, H_S = this_H_S)
  df$Reff_real[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
                                  theta = real_theta, q = this_q, 
                                  df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                  H_I = this_H_I, H_S = this_H_S)
  
  tot_inf <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
                                    theta = 0, q = this_q, 
                                    df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                    H_I = this_H_I, H_S = this_H_S)
  
  df$infections_averted_mod[i] <- tot_inf - compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
                                                                   theta = mod_theta, q = this_q, 
                                                                   df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                   H_I = this_H_I, H_S = this_H_S)
  
  df$infections_averted_real[i] <- tot_inf - compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
                                                                    theta = real_theta, q = this_q, 
                                                                    df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                    H_I = this_H_I, H_S = this_H_S)
  
  num_tests_mod <- compute_num_tests(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                     theta = mod_theta, freq = low_freq, inf_period = 1/gamma,
                                     compliance = high_compliance, q = this_q, 
                                     df$psi[i], this_X_I, this_X_S, this_H_I, this_H_S)
  
  num_tests_real <- compute_num_tests(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                      theta = real_theta, freq = low_freq, inf_period = 1/gamma,
                                      compliance = low_compliance, q = this_q, 
                                      df$psi[i], this_X_I, this_X_S, this_H_I, this_H_S)
  
  df$infections_averted_per100_mod[i] <- df$infections_averted_mod[i]/num_tests_mod*100
  df$infections_averted_per100_real[i] <- df$infections_averted_real[i]/num_tests_real*100
}

proc.time() - ptm
#df[is.na(df)] <- 0 # fix NAs (from dividing by 0 when there are no tests administered)


#saveRDS(df,file="df_fig4_setupparams.RData")

# _____________________________________________________________________
#* II: Plot fig4 ####
# _____________________________________________________________________

infectavert_mod <- ggplot(df, aes(x = phi*100, y = psi*100, fill = infections_averted_mod)) + 
  geom_tile() +
  stat_contour(aes(z = Reff_mod), breaks = 1, size = 0.6, col = "white") + 
  stat_contour(aes(z = Reff), breaks = 1, size = 0.6, col = mylightgray) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + #ylab("Infection-acquired immunity (%)") +
  xlab("") + #xlab("Population vaccination rate (%)") + 
  ggtitle("Weekly testing, 99% compliance", "")+#"Infections averted\nrel. to no testing") + 
  labs(fill = "") + 
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,12000)) + #14000 for R0=6
  theme(legend.text = element_text(size = 11), 
        legend.spacing.x = unit(0.75, 'cm'),
        plot.title = element_text(color = myblue),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_blank()) + 
  coord_fixed(1)

infectavert_real <- ggplot(df, aes(x = phi*100, y = psi*100, fill = infections_averted_real)) + 
  geom_tile() +
  stat_contour(aes(z = Reff_real), breaks = 1, size = 0.6, col =  "white") + 
  stat_contour(aes(z = Reff), breaks = 1, size = 0.6, col = mylightgray) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + #ylab("Infection-acquired immunity (%)") +
  xlab("") + #xlab("Population vaccination rate (%)") + 
  ggtitle("Weekly testing, 50% compliance", "") +# "Infections averted\nrel. to no testing") + 
  labs(fill = "") + 
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,12000)) + #14000 for R0=6
  theme(legend.position = "none",
        plot.title = element_text(color = myyellow),
        plot.subtitle = element_text(hjust = 0.5)) + 
  coord_fixed(1)

infper100_mod <- ggplot(df, aes(x = phi*100, y = psi*100, fill = infections_averted_per100_mod)) + 
  geom_tile() +
  stat_contour(aes(z = Reff_mod), breaks = 1, size = 0.6, col =  "white") + 
  stat_contour(aes(z = Reff), breaks = 1, size = 0.6, col = mylightgray) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + #ylab("Infection-acquired immunity (%)") +
  xlab("") + #xlab("Population vaccination rate (%)") + 
  ggtitle("") + #subtitle = "Infections averted per 100 tests") + 
  labs(fill = "") + 
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,5)) + # 8 for R0 = 6
  theme(legend.text = element_text(size = 11), 
        legend.spacing.x = unit(0.75, 'cm'),
        legend.text.align = 0,
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_blank()) + 
  coord_fixed(1)


infper100_real <- ggplot(df, aes(x = phi*100, y = psi*100, fill = infections_averted_per100_real)) + 
  geom_tile() +
  stat_contour(aes(z = Reff_real), breaks = 1, size = 0.6, col = "white") + 
  stat_contour(aes(z = Reff), breaks = 1, size = 0.6, col = mylightgray) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + #ylab("Infection-acquired immunity (%)") +
  xlab("") + #xlab("Population vaccination rate (%)") + 
  ggtitle("")+ #subtitle = "Infections averted per 100 tests") + 
  labs(fill = "") + 
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,5)) + 
  theme(legend.position = "none",
        plot.subtitle = element_text(hjust = 0.5)) + 
  coord_fixed(1)

fig4 <- ggarrange(infectavert_real, NULL, infectavert_mod, NULL,
                  NULL,NULL,NULL, NULL,
                  infper100_real, NULL, infper100_mod, NULL,
          ncol = 4, nrow = 3,
          widths = c(1, -0.05, 1.178, 0.05),
          heights = c(1, -0.11, 1),
          align = "hv")

annotate_figure(fig4,
                bottom = text_grob("Population vaccination rate (%)", size = 14, family = "Arial",
                                   vjust = -1.2),
                left = text_grob("Infection-acquired immunity(%)", size = 14, family = "Arial", rot = 90,
                                 vjust = 1.1))

ggsave("suppfig4_heatmap_R06.pdf", device = cairo_pdf, width = 8, height = 7.5)
#ggsave("fig4_heatmap.svg", device = svg, width = 8, height = 7.5)

