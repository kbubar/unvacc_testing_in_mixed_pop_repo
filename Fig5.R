source("setup.R")

# _____________________________________________________________________
# FIGURE 5: ####
# Total infections averted and percent reduction in infections for
# all three VE scenarios (waning, baseline, boosted) and
# both testing scenarios (weekly, 50% and 99% compliance)
#
#*
#* I: Get data ####
#* If you want to generate data yourself, start here! Otherwise, skip to line 117 to use pre-generated dataframes.
# _____________________________________________________________________
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

scenarios = c("baseline","waning","boosted")

for (s in scenarios){
  
  if(s == "baseline"){
    this_VE_I <- baseline_VE_I
    this_VE_S <- baseline_VE_S
    this_VE_P <- baseline_VE_P
    this_H_I  <- baseline_H_I
    this_H_S  <- baseline_H_S
    this_H_P  <- baseline_H_P
    this_X_S  <- delta_X_S
    this_X_I  <- delta_X_I
    this_X_P  <- delta_X_P
    hosp_rate <- infection_hosp_rate_delta
  }
  if(s == "waning"){
    this_VE_I <- waning_VE_I
    this_VE_S <- waning_VE_S
    this_VE_P <- waning_VE_P
    this_H_I  <- waning_H_I
    this_H_S  <- waning_H_S
    this_H_P  <- waning_H_P
    this_X_S  <- delta_X_S
    this_X_I  <- delta_X_I
    this_X_P  <- delta_X_P
    hosp_rate <- infection_hosp_rate_delta
  }
  if(s == "boosted"){
    this_VE_I <- boosted_VE_I
    this_VE_S <- boosted_VE_S
    this_VE_P <- boosted_VE_P
    this_H_I  <- boosted_H_I
    this_H_S  <- boosted_H_S
    this_H_P  <- boosted_H_P
    this_X_S  <- delta_X_S
    this_X_I  <- delta_X_I
    this_X_P  <- delta_X_P
    hosp_rate <- infection_hosp_rate_delta
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
  
  if(s == "baseline"){
    baselinedf <- df
  }
  if(s == "waning"){
    waningdf <- df
  }
  if(s == "boosted"){
    boosteddf <- df
  }
}
proc.time() - ptm
# If you want to save the dataframe that was run, uncomment the lines below
# saveRDS(baselinedf,file="df_Fig5_baseline.RData")
# saveRDS(waningdf,file="df_Fig5_waning.RData")
# saveRDS(boosteddf,file="df_Fig5_boosted.RData")

# If you didn't generate new dateframes, read in saved dataframes
waningdf <- readRDS("dataframes/df_Fig5_waning.RData")
baselinedf <- readRDS("dataframes/df_Fig5_baseline.RData")
boosteddf <- readRDS("dataframes/df_Fig5_boosted.RData")

# _____________________________________________________________________
#* II: Plot fig 5 ####
# _____________________________________________________________________
for (i in 1:3) {
  if (i == 1) {
    df <- waningdf
  } else if (i==2) {
    df <- baselinedf
  } else {
    df <- boosteddf
  }
  percentreduc50 <- ggplot(df, aes(x = phi*100, y = psi*100)) +
    geom_raster(aes(fill = percent_reduc_inf_50)) +
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
  
  percentreduc99 <- ggplot(df, aes(x = phi*100, y = psi*100)) +
    geom_raster(aes(fill = percent_reduc_inf_99)) +
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
          plot.subtitle = element_blank())
  
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
plot(fig5)

ggsave("Fig5.pdf", fig5, device = cairo_pdf, width = 8, height = 5)
ggsave("Fig.svg", fig5, device = svg, width = 8, height = 5)
