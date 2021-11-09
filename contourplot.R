# TEST of contour plot
source("setup.R") 

phi_vec <- seq(0, 1, by = 0.01)
psi_vec <- seq(0, 1, by = 0.01)
df <- expand.grid(phi = phi_vec, psi = psi_vec)

# _____________________________________________________________________
# Reff - infections with no testing ####
# _____________________________________________________________________
df$Reff <- NA

for (i in 1:dim(df)[1]){
  df$Reff[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,  
                             theta = 0, q = 0, 
                             df$psi[i], X_I = this_X_I, X_S = this_X_S,
                             H_I = this_H_I, H_S = this_H_S)
}

p <- ggplot(df, aes(x = phi*100, y = psi*100, z = Reff, colour = ..level..)) + 
  stat_contour(breaks = 1:R0, size = 1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") + 
  ggtitle(expression(R[eff]))

p_Reff <- direct.label(p, list(last.points, hjust = 1, vjust = -2))

# _____________________________________________________________________
# Reff - infections with testing ####
# _____________________________________________________________________
df$Reff_ideal <- NA
df$Reff_mod <- NA
df$Reff_real <- NA

for (i in 1:dim(df)[1]){
  df$Reff_ideal[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,  
                                   theta = ideal_theta, q = 0, 
                                   df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                   H_I = this_H_I, H_S = this_H_S)
  df$Reff_mod[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
                                 theta = mod_theta, q = 0, 
                                 df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                 H_I = this_H_I, H_S = this_H_S)
  df$Reff_real[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
                                  theta = real_theta, q = 0, 
                                  df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                  H_I = this_H_I, H_S = this_H_S)
}

p <- ggplot(df, aes(x = phi*100, y = psi*100, z = Reff_ideal, colour = ..level..)) + 
  stat_contour(size = 1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") + 
  ggtitle(expression(R[eff]), "99% compliance, 2x weekly") 

p_Reff_ideal <- direct.label(p, list(last.points, hjust = 1, vjust = -2))

# _____________________________________________________________________
# Percent breakthrough ####
# _____________________________________________________________________
df$breakthrough <- NA

for (i in 1:dim(df)[1]){
  df$breakthrough[i] <- compute_percent_breakthrough_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
                                                        theta = 0, q = 0, 
                                                        df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                        H_I = this_H_I, H_S = this_H_S)
}

p_breakthrough <- ggplot(df, aes(x = phi*100, y = psi*100, z = breakthrough)) + 
  geom_tile(aes(fill = breakthrough)) +
 # stat_contour(breaks = seq(10, 100, by = 10), size = 1) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") + 
  ggtitle("Breakthrough infection (%)") +
  labs(fill = "") + 
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"))

# _____________________________________________________________________
# Dominant transmission ####
# _____________________________________________________________________
df$dom_transmission <- NA

for (i in 1:dim(df)[1]){
  df$dom_transmission[i] <- compute_dominant_transmission(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
                                                          theta = 0, q = 0, 
                                                          df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                          H_I = this_H_I, H_S = this_H_S)
}

p_dom <- ggplot(df, aes(x = phi*100, y = psi*100, z = dom_transmission)) + 
  geom_tile(aes(fill = dom_transmission)) +
  # stat_contour(breaks = seq(10, 100, by = 10), size = 1) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") + 
  ggtitle("Dominant mode of transmission") + 
  labs(fill = "") 

ggarrange(p_Reff, p_breakthrough, p_dom,
          nrow = 1,
          align = "hv")

ggsave("heatmaps1.pdf", device = cairo_pdf, width = 12, height = 4)

# _____________________________________________________________________
# Number infections averted per 100 tests ####
# _____________________________________________________________________
df$infections_averted_per100_ideal <- NA
df$infections_averted_per100_mod <- NA
df$infections_averted_per100_real <- NA

for (i in 1:dim(df)[1]){
  df$infections_averted_per100_ideal[i] <- compute_infections_averted_per100tests(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, 
                                                                                  theta = ideal_theta, q = 0, 
                                                                                  df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                                  H_I = this_H_I, H_S = this_H_S,
                                                                                  freq = high_freq, compliance = high_compliance)
  df$infections_averted_per100_mod[i] <- compute_infections_averted_per100tests(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                                                theta = mod_theta, q = 0, 
                                                                                df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                                                H_I = this_H_I, H_S = this_H_S,
                                                                                freq = low_freq, compliance = high_compliance)
  df$infections_averted_per100_real[i] <- compute_infections_averted_per100tests(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                                                 theta = real_theta, q = 0,
                                                                                 df$psi[i],  X_I = this_X_I, X_S = this_X_S,
                                                                                 H_I = this_H_I, H_S = this_H_S,
                                                                                 freq = low_freq, compliance = low_compliance)
}

p_ideal <- ggplot(df, aes(x = phi*100, y = psi*100, fill = infections_averted_per100_ideal)) + 
  geom_tile() +
  #stat_contour(aes(z = Reff_ideal), breaks = c(1), size = 1) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") + 
  ggtitle("2x weekly testing, 99% compliance") + 
  labs(fill = "") + 
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,5))


p_mod <- ggplot(df, aes(x = phi*100, y = psi*100, fill = infections_averted_per100_mod)) + 
  geom_tile() +
  stat_contour(aes(z = Reff_mod), breaks = 1, size = 1) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") + 
  ggtitle("Weekly testing, 99% compliance") + 
  labs(fill = "") + 
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,5))

p_real <- ggplot(df, aes(x = phi*100, y = psi*100, fill = infections_averted_per100_real)) + 
  geom_tile() +
  stat_contour(aes(z = Reff_real), breaks = 1, size = 1) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") + 
  ggtitle("Weekly testing, 50% compliance") + 
  labs(fill = "") + 
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,5))

ggarrange(p_real, p_mod, p_ideal,
          ncol = 3,
          align = "hv")

ggsave("heatmaps3.pdf", device = cairo_pdf, width = 12, height = 4)
