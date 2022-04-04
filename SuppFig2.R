source("setup.R")

# _____________________________________________________________________
# SUPP FIGURE 2: ####
# Reff, % infections in the unvaccinated and % infections driven by the unvaccinated over phi and psi
# _____________________________________________________________________
R0 <- 6
alpha <- R0*gamma/N # transmissibility

# _____________________________________________________________________
#* I: Get data ####
#* If you want to generate data yourself, start here! Otherwise, skip to line 133 to use pre-generated dataframes.
# _____________________________________________________________________
ptm <- proc.time()
phi_vec <- seq(0, 1, by = 0.05) # fine grid : by = 0.01 (~2 hr)
psi_vec <- seq(0, 1, by = 0.05)
df <- expand.grid(phi = phi_vec, psi = psi_vec)

df$Reff <- NA
df$tot_hosp <- NA
df$breakthrough <- NA
df$dom_transmission <- NA
df$tot_infections <- NA
df$breakthrough_hosp <- NA

scenarios = c("baseline","waning","boosted","omicron")

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
  if(s == "omicron"){
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
  }
  
  for (i in 1:dim(df)[1]){
    
    df$Reff[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                               theta = 0, q = this_q,
                               df$psi[i], X_I = this_X_I, X_S = this_X_S,
                               H_I = this_H_I, H_S = this_H_S)
    
    df$tot_infections[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
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
    
    df$tot_hosp[i] <- compute_tot_hospitalizations(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P,
                                                   infection_hosp_rate = hosp_rate,
                                                   theta = 0, q = this_q,
                                                   df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                   H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
    
    df$breakthrough_hosp[i] <- compute_percent_breakthrough_hosp(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P,
                                                                 infection_hosp_rate = hosp_rate,
                                                                 theta = 0, q = this_q,
                                                                 df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                                 H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
  }
  
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
  
}
proc.time() - ptm
# If you want to save the dataframe that was run, uncomment the lines below
# saveRDS(baselinedf,file="df_SuppFig2_baseline.RData")
# saveRDS(waningdf,file="df_SuppFig2_waning.RData")
# saveRDS(boosteddf,file="df_SuppFig2_boosted.RData")
# saveRDS(omicrondf,file="df_SuppFig2_omicron.RData")

# If you didn't generate new dateframes, read in saved dataframes
baselinedf <- readRDS("dataframes/df_SuppFig2_baseline_R06.RData")
waningdf <- readRDS("dataframes/df_SuppFig2_waning_R06.RData")
boosteddf <- readRDS("dataframes/df_SuppFig2_boosted_R06.RData")
omicrondf <- readRDS("dataframes/df_SuppFig2_omicron_R06.RData")

# _____________________________________________________________________
#* II: Plot ####
# _____________________________________________________________________
theme_set(theme_minimal(base_size = 11))
theme_update(text = element_text(family="Arial", size = 11),
             plot.title = element_text(size = 11, hjust = 0.5, family="Arial"))

#* plot infections heatmap with Reff contours ####
p_Reff_inf <- ggplot(baselinedf, aes(x = phi*100, y = psi*100, z = Reff)) + 
  geom_raster(aes(fill = tot_infections)) +
  geom_contour(breaks = 1:R0, size = 0.4, color = "white") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Infection-acquired\nimmunity (%)") +
  xlab("Population vaccination rate (%)") +
  #ggtitle(expression(R[eff])) +
  ggtitle("\nTotal infections") +
  scale_fill_viridis(option="viridis", limits = c(0, N)) +
  coord_fixed(1) +
  labs(fill = "") +
  theme(legend.text = element_text(size = 10),
        legend.spacing.x = unit(0.15, 'cm'))

#* plot hospitalizations heatmap with Reff contours ####
p_Reff_hosp <- ggplot(baselinedf, aes(x = phi*100, y = psi*100, z = Reff)) +
  geom_raster(aes(fill = tot_hosp)) +
  geom_contour(breaks = 1:R0, size = 0.4, color = "white") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  ylab("Infection-acquired\nimmunity (%)") +
  xlab("Population vaccination rate (%)") +
  #ggtitle(expression(R[eff])) +
  ggtitle("\nTotal hospitalizations") +
  scale_fill_viridis(option="viridis", limits = c(0, 400)) +
  coord_fixed(1) +
  labs(fill = "") +
  theme(legend.text = element_text(size = 10),
        legend.spacing.x = unit(0.15, 'cm'))

#* plot % infections in the unvaccinated ####
p_infection <- ggplot(baselinedf, aes(x = phi*100, y = psi*100, z = breakthrough)) +
  geom_raster(aes(fill = 100 - breakthrough)) +
  stat_contour(breaks = c(50), size = 0.4, col = "white") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") +
  ggtitle("% of infections among\nunvaccinated individuals") +
  labs(fill = "Percent\ninfections\nin vacc.") +
  theme(legend.position = "none") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno")) +
  coord_fixed(1)

#* plot dominant transmission ####
p_transmission <- ggplot(baselinedf, aes(x = phi*100, y = psi*100, z = dom_transmission)) +
  geom_raster(aes(fill = dom_transmission)) +
  stat_contour(breaks = c(50), size = 0.4, color = "white") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + # ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") +
  ggtitle("% of transmission from\nunvaccinated individuals") +
  labs(fill = "")+ #"Percent of\ntransmission\nby unvacc.") +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.title.align = 0.5,
        legend.spacing.x = unit(0.15, 'cm')) +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"))+
  coord_fixed(1)

#* plot % hospitalizations in the unvaccinated ####
p_hospitalization <- ggplot(baselinedf, aes(x = phi*100, y = psi*100, z = breakthrough_hosp)) +
  geom_raster(aes(fill = 100 - breakthrough_hosp)) +
  stat_contour(breaks = c(50), size = 0.4, col = "white") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + # ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") +
  ggtitle("% of hospitalizations among\nunvaccinated individuals") +
  labs(fill = "Percent\nhospitalizations\nin vacc.") +
  theme(legend.position = "none") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno")) +
  coord_fixed(1)

inf_legend <- get_legend(p_Reff_inf)
p_Reff_inf <- p_Reff_inf + theme(legend.position = "none")

hosp_legend <- get_legend(p_Reff_hosp)
p_Reff_hosp <- p_Reff_hosp + theme(legend.position = "none")

percent_legend <- get_legend(p_transmission)
p_transmission <- p_transmission + theme(legend.position = "none")

suppfig2toprow <- ggarrange(NULL,p_Reff_inf, NULL, inf_legend, NULL, p_Reff_hosp, NULL, hosp_legend,
                     labels = c(NA, "a", NA, NA, "b", NA, NA, NA),
                     nrow = 1,
                     ncol = 8,
                     #align = "hv",
                     widths = c(0.1, 1, -0.05, 0.5, 0.01, 1, -0.05, 0.5),
                     label.y = 0.95)

suppfig2bottomrow <- ggarrange(p_infection, NULL, p_transmission, NULL, p_hospitalization, percent_legend,
                        labels = c("c", NA, "d", NA, "e", NA),
                        nrow = 1,
                        ncol = 6,
                        #align = "hv",
                        widths = c(1, 0.05, 1, 0.05, 1, 0.3),
                        label.y = 1)


lay <- cbind(c(1, 2))

suppfig2 <- arrangeGrob(suppfig2toprow, suppfig2bottomrow, layout_matrix = lay,
                    heights = c(1, 1))
plot(suppfig2)

ggsave("SuppFig2.pdf", suppfig2, device = cairo_pdf, width = 8, height = 5)
ggsave("SuppFig2.png", suppfig2, device = png, width = 8, height = 5)
ggsave("SuppFig2.svg", suppfig2, device = svg, width = 8, height = 5)