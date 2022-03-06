source("setup.R")


# _____________________________________________________________________
# SUPP FIG1 - same as Fig 2 with R0 = 6 ####
# _____________________________________________________________________

# Run in contourplot.R


# _____________________________________________________________________
# SUPP FIG2 - heatmaps of total infections and Reff contours, R0 = 4,6 ####
# _____________________________________________________________________

for (thisrow in 1:2){
  if (thisrow == 1){
    baselinedf <- readRDS("df_fig2_baseline.RData")
    waningdf <- readRDS("df_fig2_waning.RData")
    boosteddf <- readRDS("df_fig2_boosted.RData")
  } else {
    baselinedf <- readRDS("df_suppfig2_baseline_R06.RData")
    waningdf <- readRDS("df_suppfig2_waning_R06.RData")
    boosteddf <- readRDS("df_suppfig2_boosted_R06.RData")
  }
  
  p_waning <- ggplot(waningdf, aes(x = phi*100, y = psi*100, z = Reff))+ 
    geom_tile(aes(fill = tot_infections)) +
    geom_contour(breaks = 1:R0, size = 0.4, color = "white") +
    geom_text_contour(breaks = 1:R0, color = "white", rotate = FALSE, nudge_y = 1, nudge_x = 2, skip = 0)+
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    ylab("") +
    xlab("") +# xlab("Population vaccination rate (%)") + 
    scale_fill_viridis(option="viridis", limits = c(0, N)) +
    coord_fixed(1) + 
    theme(legend.position = "none",
          axis.title.y = element_blank()) 
  
  p_baseline <- ggplot(baselinedf, aes(x = phi*100, y = psi*100, z = Reff))+ #, colour = ..level..)) + 
    geom_tile(aes(fill = tot_infections)) +
    geom_contour(breaks = 1:R0, size = 0.4, color = "white") +
    geom_text_contour(breaks = 1:R0, color = "white", rotate = FALSE, nudge_y = 1, nudge_x = 2, skip = 0)+
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    ylab("") +
    xlab("") + 
    scale_fill_viridis(option="viridis", limits = c(0, N)) +
    coord_fixed(1) + 
    theme(legend.position = "none", 
          axis.title.y = element_blank()) 
  
  p_boosted <- ggplot(boosteddf, aes(x = phi*100, y = psi*100, z = Reff))+ #, colour = ..level..)) + 
    geom_tile(aes(fill = tot_infections)) +
    geom_contour(breaks = 1:R0, size = 0.4, color = "white") +
    geom_text_contour(breaks = 1:R0, color = "white", rotate = FALSE, nudge_y = 1, nudge_x = 2, skip = 0)+
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    ylab("") +
    xlab("") + # xlab("Population vaccination rate (%)") 
    scale_fill_viridis(option="viridis", limits = c(0, N)) +
    coord_fixed(1) + 
    labs(fill = "") +
    theme(legend.text = element_text(size = 11), 
          legend.spacing.x = unit(0.7, 'cm'),
          axis.title.y = element_blank()) 
  
  num_legend <- get_legend(p_boosted)
  
  if (thisrow == 1){
    p_waning_R04 <- p_waning + ggtitle("Waning/low VE") + onlyy_theme
    p_baseline_R04 <- p_baseline + ggtitle("Baseline VE") + nolabels_theme
    p_boosted_R04 <- p_boosted + ggtitle("Boosted/high VE") + theme(legend.position = "none") + 
                    nolabels_theme
  } else {
    p_waning_R06 <- p_waning
    p_baseline_R06 <- p_baseline + onlyx_theme + xlab("Population vaccination rate (%)")
    p_boosted_R06 <- p_boosted + theme(legend.position = "none")  + onlyx_theme
  }
}

panels <- ggarrange(p_waning_R04, NULL, p_baseline_R04, NULL, p_boosted_R04,
                    NULL, NULL, NULL, NULL, NULL,
                    p_waning_R06, NULL, p_baseline_R06, NULL, p_boosted_R06,
                    nrow = 3, ncol = 5,
                    align = "hv",
                    widths = c(1, -0.14, 1, -0.14, 1),
                    heights = c(1, -0.22, 1),
                    labels = c("a", NA, "    b", NA,"    c",
                               NA, NA, NA, NA, NA,
                               "d", NA, "    e", NA, "    f"),
                    label.y = 0.88)

lay <- rbind(c(1, 2))

suppfig2 <- arrangeGrob(panels, num_legend, layout_matrix = lay,
                    widths = c(3, 0.5), 
                    left = c("Infection-acquired immunity (%)"))

ggsave("SuppFig2.pdf", suppfig2, device = cairo_pdf, width = 8, height = 5)
ggsave("SuppFig2.svg", suppfig2, device = svg, width = 8, height = 5)


# _____________________________________________________________________
# SUPP FIG3 - same as Fig 5 with R0 = 6 ####
# _____________________________________________________________________

# Run in contourplot.R


# _____________________________________________________________________
# SUPP FIG5 - same as Fig 5 with hospitalizations ####
# _____________________________________________________________________
# Either read in the corresponding RDS file

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
df$hosp_notesting <- NA
df$hosp_99 <- NA
df$hosp_50 <- NA

scenarios = c("baseline","waning","boosted","omicron")

for (s in scenarios){
  
  if(s == "baseline"){
    this_VE_I <- baseline_VE_I
    this_VE_S <- baseline_VE_S
    this_VE_P <- baseline_VE_P
    this_H_I  <- baseline_H_I
    this_H_S  <- baseline_H_S
    this_H_P  <- baseline_H_P
  }
  if(s == "waning"){
    this_VE_I <- waning_VE_I
    this_VE_S <- waning_VE_S
    this_VE_P <- waning_VE_P
    this_H_I  <- waning_H_I
    this_H_S  <- waning_H_S
    this_H_P  <- waning_H_P
  }
  if(s == "boosted"){
    this_VE_I <- boosted_VE_I
    this_VE_S <- boosted_VE_S
    this_VE_P <- boosted_VE_P
    this_H_I  <- boosted_H_I
    this_H_S  <- boosted_H_S
    this_H_P  <- boosted_H_P
  }
  if(s == "omicron"){
    this_VE_I <- omicron_VE_I
    this_VE_S <- omicron_VE_S
    this_VE_P <- omicron_VE_P
    this_H_I  <- omicron_H_I
    this_H_S  <- omicron_H_S
    this_H_P  <- omicron_H_P
  }

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
    
    df$hosp_notesting[i] <- compute_tot_hospitalizations(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P, 
                                 infection_hosp_rate = infection_hosp_rate_omicron,
                                 theta = 0, q = this_q, 
                                 df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                 H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
    
    df$hosp_99[i] <- compute_tot_hospitalizations(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P, 
                                  infection_hosp_rate = infection_hosp_rate_omicron,
                                  theta = theta_99, q = this_q, 
                                  df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                  H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
    
    df$hosp_50[i] <- compute_tot_hospitalizations(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P, 
                                  infection_hosp_rate = infection_hosp_rate_omicron,
                                  theta = theta_50, q = this_q, 
                                  df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                  H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
  }
  
  df$percent_reduc_hosp_99 <- (df$hosp_notesting - df$hosp_99)/df$hosp_notesting*100
  df$percent_reduc_hosp_50 <- (df$hosp_notesting - df$hosp_50)/df$hosp_notesting*100

  if(s == "baseline"){
    baselinedf <- df
  }
  if(s == "waning"){
    waningdf <- df
  }
  if(s == "boosted"){
    boosteddf <- df
  }
  if(s == "omicron"){
    omicrondf <- df
  }
  #saveRDS(df,file="df_Fig2_baseline.RData")
  
}

proc.time() - ptm
#saveRDS(df,file="df_suppFig5_boosted_R04_testeveryone.RData")

#* II: Plot fig5 ####

percentreduc50_waning <- ggplot(waningdf, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) +
  geom_tile(aes(fill = percent_reduc_hosp_50)) +
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

percentreduc50_baseline <- ggplot(baselinedf, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) +
  geom_tile(aes(fill = percent_reduc_hosp_50)) +
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

percentreduc50_boosted <- ggplot(boosteddf, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) +
  geom_tile(aes(fill = percent_reduc_hosp_50)) +
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

percentreduc99_waning <- ggplot(waningdf, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) +
  geom_tile(aes(fill = percent_reduc_hosp_99)) +
  geom_contour(aes(z = Reff_99), breaks = 1, size = 0.6, col = "white") +
  geom_contour(aes(z = Reff), breaks = 1, size = 0.6, col = "white", linetype = "longdash") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") +
  ggtitle("Weekly testing, 99% compliance", "% reduction in hospitalizations due to testing") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) +
  coord_fixed(1) +
  labs(fill = "") +
  theme(axis.title.y = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank()) #element_text(hjust = 0.5))

percentreduc99_baseline <- ggplot(baselinedf, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) +
  geom_tile(aes(fill = percent_reduc_hosp_99)) +
  geom_contour(aes(z = Reff_99), breaks = 1, size = 0.6, col = "white") +
  geom_contour(aes(z = Reff), breaks = 1, size = 0.6, col = "white", linetype = "longdash") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") +
  ggtitle("Weekly testing, 99% compliance", "% reduction in hospitalizations due to testing") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) +
  coord_fixed(1) +
  labs(fill = "") +
  theme(axis.title.y = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank()) #element_text(hjust = 0.5))

percentreduc99_boosted <- ggplot(boosteddf, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) +
  geom_tile(aes(fill = percent_reduc_hosp_99)) +
  geom_contour(aes(z = Reff_99), breaks = 1, size = 0.6, col = "white") +
  geom_contour(aes(z = Reff), breaks = 1, size = 0.6, col = "white", linetype = "longdash") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("") +# xlab("Population vaccination rate (%)") +
  ggtitle("Weekly testing, 99% compliance", "% reduction in hospitalizations due to testing") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) +
  coord_fixed(1) +
  labs(fill = "") +
  theme(axis.title.y = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank()) #element_text(hjust = 0.5))

percent_legend <- get_legend(percentreduc99_baseline)

percentreduc50_waning <- percentreduc50_waning + onlyy_theme + ggtitle("Waning/low VE")
percentreduc99_waning <- percentreduc99_waning + theme(legend.position = "none")
percentreduc50_baseline <- percentreduc50_baseline + nolabels_theme + ggtitle("Baseline VE")
percentreduc99_baseline <- percentreduc99_baseline + onlyx_theme + 
  theme(legend.position = "none", plot.title = element_blank()) + 
  xlab("Population vaccination rate (%)")
percentreduc50_boosted <- percentreduc50_boosted + nolabels_theme + ggtitle("Boosted/high VE")
percentreduc99_boosted <- percentreduc99_boosted + onlyx_theme + theme(legend.position = "none", plot.title = element_blank())



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

fig5_supp <- arrangeGrob(panels, percent_legend, layout_matrix = lay,
                    widths = c(3, 0.5), 
                    left = c("Infection-acquired immunity (%)"))

ggsave("suppFig5_hospitalizations.pdf", fig5_supp, device = cairo_pdf, width = 8, height = 5)
ggsave("suppFig5.svg", fig5, device = svg, width = 8, height = 5)

