source("setup.R")

# _____________________________________________________________________
# SUPP FIGURE 4: ####
# Percent reduction in infections and infected time series for
# plausible omicron scenarios assuming three testing
# scenarios (weekly 50% and 99% compliance, twice weekly 99% compliance)
#
# Same as main text fig 6 w/R0 = 6
# _____________________________________________________________________
this_VE_I <- omicron_VE_I
this_VE_S <- omicron_VE_S
this_VE_P <- omicron_VE_P
this_H_I  <- omicron_H_I
this_H_S  <- omicron_H_S
this_H_P  <- omicron_H_P
this_X_S  <- omicron_X_S
this_X_I  <- omicron_X_I
this_X_P  <- omicron_X_P
hosp_rate <- infection_hosp_rate_omicron

R0 <- 6
alpha <- R0*gamma/N # transmissibility

# _____________________________________________________________________
#* I: Get data ####
#* If you want to generate data yourself, start here! Otherwise, skip to line 95 to use pre-generated dataframes.
# _____________________________________________________________________
ptm <- proc.time()

phi_vec <- seq(0, 1, by = 0.05) # fine grid : by = 0.01
psi_vec <- seq(0, 1, by = 0.05)
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

# If you want to save the dataframe that was run, uncomment the lines below
#saveRDS(df,file="df_SuppFig4.RData")

# If you didn't generate new dateframes, read in saved dataframes
df <- readRDS("dataframes/df_SuppFig6_R06.RData")

# _____________________________________________________________________
#* II: Plot fig6 ####
# _____________________________________________________________________
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

plot_timeseries <- ggplot(df_timeseries, aes(x = time)) +
  geom_line(aes(y = notesting/1000), col = "black", size = 0.8) +
  geom_line(aes(y = testing50/1000), col = theta50_purple, size = 0.8) +
  geom_line(aes(y = testing99/1000), col = theta99_purple, size = 0.8) +
  geom_line(aes(y = testing99_biwk/1000), col = thetabiwk_purple, size = 0.8) +
  ggtitle("\n") + # just for formatting
  ylab("Infected (#)") +
  xlab("Time (days)") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 200)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5000/1000)) +
  alllabels_theme +
  coord_fixed(200/5) +
  theme(plot.margin = margin(10, 10, 10, 10))

percentreduc50 <- ggplot(df, aes(x = phi*100, y = psi*100)) +
  geom_raster(aes(fill = percent_reduc_inf_50)) +
  geom_contour(aes(z = Reff_50), breaks = 1, size = 0.6, color = "white") +
  geom_contour(aes(z = Reff), breaks = 1, size = 0.6, col = "white", linetype = "longdash") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_point(aes(x = 58, y = 35), color = "white", size = 0.2) +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") +
  ggtitle("") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) +
  coord_fixed(1) +
  theme(legend.position = "none")

percentreduc99 <- ggplot(df, aes(x = phi*100, y = psi*100)) + 
  geom_raster(aes(fill = percent_reduc_inf_99)) +
  geom_contour(aes(z = Reff_99), breaks = 1, size = 0.6, col = "white") +
  geom_contour(aes(z = Reff), breaks = 1, size = 0.6, col = "white", linetype = "longdash") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_point(aes(x = 58, y = 35), color = "white", size = 0.2) +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") +
  ggtitle("Weekly testing, 99% compliance", "% reduction in infections due to testing") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) +
  coord_fixed(1) +
  labs(fill = "") +
  theme(axis.title.y = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank()) #element_text(hjust = 0.5))

percentreducbiwk <- ggplot(df, aes(x = phi*100, y = psi*100)) + 
  geom_raster(aes(fill = percent_reduc_inf_biwk)) +
  geom_contour(aes(z = Reff_biwk), breaks = 1, size = 0.6, col = "white") +
  geom_contour(aes(z = Reff), breaks = 1, size = 0.6, col = "white", linetype = "longdash") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_point(aes(x = 58, y = 35), color = "white", size = 0.2) +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") +
  ggtitle("Biweekly testing, 99% compliance", "% reduction in infections due to testing") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) +
  coord_fixed(1) +
  labs(fill = "") +
  theme(axis.title.y = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank()) #element_text(hjust = 0.5))

percentreducbiwk <- addSmallLegend(percentreducbiwk)
percent_legend <- get_legend(percentreducbiwk)

percentreduc50_baseline <- percentreduc50 + alllabels_theme + ggtitle("Weekly testing,\n50% Compliance")
percentreduc99_baseline <- percentreduc99 + onlyx_theme +
  theme(legend.position = "none") + ggtitle("Weekly testing,\n99% Compliance") +
  xlab("Population vaccination rate (%)")
percentreducbiwk_baseline <- percentreducbiwk + onlyx_theme + ggtitle("Biweekly testing,\n99% Compliance")

panels <- ggarrange(percentreduc50_baseline, NULL,
                    percentreduc99_baseline, NULL,
                    percentreducbiwk_baseline,
                    nrow = 1, ncol = 5,
                    align = "hv",
                    widths = c(1, -0.16, 1, -0.16, 1),
                    heights = c(1),
                    labels = c("    a", "b", NA,"c", NA),
                    label.y = 1)

timeseries <- ggarrange(NULL, plot_timeseries, NULL,
                        nrow = 1, ncol = 3,
                        widths = c(0.2, 1, 0.2),
                        heights = c(1),
                        align = "hv",
                        labels = c(NA, "d", NA))

lay <- rbind(c(1, 2, 3))

suppfig4 <- arrangeGrob(panels, percent_legend, plot_timeseries, layout_matrix = lay,
                    widths = c(2.8, 0.2, 1))
plot(suppfig4)

ggsave("SuppFig4.pdf", suppfig4, device = cairo_pdf, width = 8, height = 2.5)
ggsave("SuppFig4.svg", suppfig4, device = svg, width = 8, height = 2.5)
