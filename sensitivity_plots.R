# TEST of contour plot
source("setup.R") 

######################################################################################################
##########################       Scenarios        ####################################
######################################################################################################

# Standard params
this_VE_S = 0.7; this_VE_I = 0.2;
this_H_S = (1-this_X_S)*this_VE_S + this_X_S
this_H_I = (1-this_X_I)*this_VE_I + this_X_I

# Scenario 1 : boosted VE
this_VE_S = 0.873; this_VE_I = 0.608;
this_H_S = (1-this_X_S)*this_VE_S + this_X_S
this_H_I = (1-this_X_I)*this_VE_I + this_X_I

# Scenario 2 : low VE
this_VE_S = 0.3; this_VE_I = 0.1;
this_H_S = (1-this_X_S)*this_VE_S + this_X_S
this_H_I = (1-this_X_I)*this_VE_I + this_X_I

phi_vec <- seq(0, 1, by = 0.05)
psi_vec <- seq(0, 1, by = 0.05)

this_theta <- 0
this_q <- 0
this_phi <- 0.58 # fully vacc. in US as of 11/4
this_psi <- 0.35  # CDC estimate 
R0 <- 4
alpha <- R0*gamma/N # transmissibility

df <- expand.grid(phi = phi_vec, psi = psi_vec)

df$Reff <- NA
df$Reff_99 <- NA
df$Reff_50 <- NA
df$breakthrough <- NA
df$dom_transmission <- NA
df$totinfections_99 <- NA
df$totinfections_50 <- NA
df$infections_averted_99 <- NA
df$infections_averted_50 <- NA
df$infections_averted_per100_99 <- NA
df$infections_averted_per100_50 <- NA

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
  
  df$infections_averted_99[i] <- df$totinfections_notesting[i] - df$totinfections_99[i]
  
  df$infections_averted_50[i] <- df$totinfections_notesting[i] - df$totinfections_50[i]
  
  num_tests_99 <- compute_num_tests(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                     theta = theta_99, freq = low_freq, inf_period = 1/gamma,
                                     compliance = high_compliance, q = this_q, 
                                     df$psi[i], this_X_I, this_X_S, this_H_I, this_H_S)
  
  num_tests_50 <- compute_num_tests(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                      theta = theta_50, freq = low_freq, inf_period = 1/gamma,
                                      compliance = low_compliance, q = this_q, 
                                      df$psi[i], this_X_I, this_X_S, this_H_I, this_H_S)
  
  df$infections_averted_per100_99[i] <- df$infections_averted_99[i]/num_tests_99*100
  df$infections_averted_per100_50[i] <- df$infections_averted_50[i]/num_tests_50*100
}




# plot Reff
p <- ggplot(df, aes(x = phi*100, y = psi*100, z = Reff, colour = ..level..)) + 
  stat_contour(breaks = 1:R0, size = 1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("") + #ylab("Infection-acquired immunity (%)") +
  xlab("") #+# xlab("Population vaccination rate (%)") #+ 
  #ggtitle(expression(R[eff])) 

p_Reff <- direct.label(p, list("smart.grid"))

# plot % infections in the unvaccinated
p_breakthrough <- ggplot(df, aes(x = phi*100, y = psi*100, z = breakthrough)) + 
  geom_tile(aes(fill = 100 - breakthrough)) +
  stat_contour(breaks = c(50), size = 0.4, col = "white") + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + # ylab("Infection-acquired immunity (%)") +
  xlab("") + 
  #ggtitle("% of infections among\nunvaccinated individuals") +
  labs(fill = "Percent\ninfections\nin vacc.") +
  theme(legend.text = element_text(size = 11), 
        legend.title = element_text(size = 11),
        legend.title.align = 0.5,
        legend.spacing.x = unit(0.5, 'cm'), 
        axis.text.y = element_blank(), 
        legend.position = "none") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,100)) 
# annotate("text", label = "50",
#           x = 65, y = 85, colour = "white")  

# plot dominant transmission
p_dom <- ggplot(df, aes(x = phi*100, y = psi*100, z = dom_transmission)) + 
  geom_tile(aes(fill = dom_transmission)) +
  stat_contour(breaks = c(50), size = 0.4, color = "white") + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + # ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") + # xlab("Population vaccination rate (%)") + 
  #ggtitle("% of transmission from\nunvaccinated individuals") + 
  labs(fill = "")+#"Percent of\ntransmission\nby unvacc.") +
  theme(legend.text = element_text(size = 11), 
        legend.title = element_text(size = 11),
        legend.title.align = 0.5,
        legend.spacing.x = unit(0.5, 'cm'),
        axis.text.y = element_blank(), 
        legend.position = "none") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,100)) #+ 


mylinesize <- 0.4
infectavert_99 <- ggplot(df, aes(x = phi*100, y = psi*100, fill = totinfections_99)) + 
  geom_tile() +
  stat_contour(aes(z = Reff_99), breaks = 1, size = mylinesize, col = "white") + 
  #stat_contour(aes(z = Reff), breaks = 1, size = mylinesize, col = mylightgray, linetype = "longdash") + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + #ylab("Infection-acquired immunity (%)") +
  xlab("") + #xlab("Population vaccination rate (%)") + 
  #ggtitle("Weekly testing,\n 99% compliance", "")+#"Infections averted\nrel. to no testing") + 
  labs(fill = "") + 
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,20000)) + #14000 for R0=6
  theme(legend.text = element_text(size = 11), 
        legend.spacing.x = unit(0.75, 'cm'),
        plot.title = element_text(color = myblue),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_blank(), 
        legend.position = "none") #+ 
  #coord_fixed(1)

infectavert_50 <- ggplot(df, aes(x = phi*100, y = psi*100, fill = totinfections_50)) + 
  geom_tile() +
  stat_contour(aes(z = Reff_50), breaks = 1, size = mylinesize, col =  "white") + 
  #stat_contour(aes(z = Reff), breaks = 1, size = mylinesize, col = mylightgray, linetype = "longdash") + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("") + #ylab("Infection-acquired immunity (%)") +
  xlab("") + #xlab("Population vaccination rate (%)") + 
  #ggtitle("Weekly testing,\n 50% compliance", "") +# "Infections averted\nrel. to no testing") + 
  labs(fill = "") + 
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,20000)) + #14000 for R0=6
  theme(legend.position = "none",
    plot.title = element_text(color = myyellow),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y = element_blank()) #+ 
  #coord_fixed(1)



ggarrange(p_Reff, p_breakthrough, p_dom, infectavert_99, infectavert_50,
          ncol = 5,
          widths = c(1, 1.15, 1.15, 1.15, 1.15),
          heights = c(1,1,1,1,1),
          #labels = c('a  ', 'b', 'c', 'd', 'e'),
          label.y = 0.92,
          align = "hv")

ggsave("Sens_lowVE.pdf", device = cairo_pdf, width = 12, height = 3)




######################################################################################################
##########################       VE Matrix Plots        ####################################
######################################################################################################

# Optimize phi function - finds the minimum value of phi at which vax dominate INFECTIONS
phi_infections <- function(phi_vec, VE_I, VE_S, theta = 0, q = 0, 
                                                    psi=0, X_I=0, X_S=0, H_I=0, H_S=0){
  idx = floor(length(phi_vec)/2)
  this_phi = phi_vec[idx]
  sweep = True; prop_v = 0;
  while (sweep){
    
    # calculate this vax fraction
    u_inf <- compute_u_infections(this_phi, VE_I, VE_S, theta = 0, q = 0, 
                                                    psi=0, X_I=0, X_S=0, H_I=0, H_S=0)
    v_inf <- compute_v_infections(this_phi, VE_I, VE_S, theta = 0, q = 0, 
                                  psi=0, X_I=0, X_S=0, H_I=0, H_S=0)
    update_prop_v <- v_inf / (u_inf + v_inf)
    
    
    ifelse(update_prop_v > 50,
           # if overshot, look at lower values
           idx = floor(idx/2),
           # if undershot, look at higher values
           idx = idx + floor(idx/2)
           )
  }

}

phi_vec <- seq(0, 1, by = 0.01)
psi_vec <- seq(0, 1, by = 0.01)

this_theta <- 0
this_q <- 0

this_phi <- 0.58 # fully vacc. in US as of 11/4
this_psi <- 0.35  # CDC estimate 

R0 <- 4
alpha <- R0*gamma/N # transmissibility

VE_I_vec = seq(0.5, 1, by=0.01)
VE_S_vec = seq(0.5, 1, by=0.01)
df <- expand.grid(VE_I = VE_I_vec, VE_S = VE_S_vec)


# _____________________________________________________________________
# Reff - infections with no testing ####
# _____________________________________________________________________
df$Reff <- NA

for (i in 1:dim(df)[1]){
  df$Reff[i] <- compute_Reff(phi = this_phi, VE_I = df$VE_I[i], VE_S = df$VE_S[i],  
                             theta = 0, q = 0, 
                             psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                             H_I = this_H_I, H_S = this_H_S)
}

p <- ggplot(df, aes(x = VE_I*100, y = VE_S*100, z = Reff, colour = ..level..)) + 
  stat_contour(breaks = linspace(0.5,4,8), size = 1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("VE against infection (%)") +
  xlab("VE against transmission (%)") + 
  ggtitle(expression(R[eff]))

p_Reff <- direct.label(p, list(last.points, hjust = 0.1, vjust = -3))

# _____________________________________________________________________
# Transition point - where does dominant transmission switch from u->v?
# _____________________________________________________________________

df$vax_dominate_transmission <- NA

for (i in 1:dim(df)[1]){

  phi_df <- data.frame(phi = phi_vec)
  
  lists_who_caused <- lapply(phi_vec, compute_who_caused_cases_tot, 
                             VE_I = df$VE_I[i], VE_S = df$VE_S[i], theta = this_theta, q = this_q, 
                             psi = this_psi, X_I = this_X_I, X_S = this_X_S, H_I = this_H_I, H_S = this_H_S)
  mat_who_caused <- matrix(unlist(lists_who_caused), ncol=6, byrow=TRUE)
  
  phi_df$cases_in_v_by_u <- mat_who_caused[,2]
  phi_df$cases_in_u_by_u <- mat_who_caused[,4]
  
  # get the phi value when cumulative infections by u drop below 50%
  df$vax_dominate_transmission[i] <- phi_df[min(which(phi_df$cases_in_u_by_u + phi_df$cases_in_v_by_u < 0.5)), 1]*100

}

p_transmission <- ggplot(df, aes(x = VE_I*100, y = VE_S*100, z = vax_dominate_transmission)) + 
  geom_tile(aes(fill = vax_dominate_transmission)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("VE against infection (%)") +
  xlab("VE against transmission (%)") + 
  ggtitle("Vaccination rate at which\n vaccinated dominate transmission") +
  labs(fill = "") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(50,100))


# _____________________________________________________________________
# Transition point - where does dominant infection switch from u->v?
# _____________________________________________________________________
df$vax_dominate_infection <- NA

for (i in 1:dim(df)[1]){
  
  phi_df <- data.frame(phi = phi_vec)
  phi_df$breakthroughs <- NA
  
  phi_df$breakthroughs <- lapply(phi_vec, compute_percent_breakthrough_infections,
                          VE_I = df$VE_I[i], VE_S = df$VE_S[i], theta = this_theta, q = this_q, 
                          psi = this_psi, X_I = this_X_I, X_S = this_X_S, H_I = this_H_I, H_S = this_H_S)
  
  # get the phi value when cumulative infections by u drop below 50%
  df$vax_dominate_infection[i] <- phi_df[min(which(phi_df$breakthroughs >= 50)), 1]*100
  
}

p_infection <- ggplot(df, aes(x = VE_I*100, y = VE_S*100, z = vax_dominate_infection)) + 
  geom_tile(aes(fill = vax_dominate_infection)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("VE against infection (%)") +
  xlab("VE against transmission (%)") + 
  ggtitle("Vaccination rate at which\n infections are predominantly breakthroughs") +
  labs(fill = "") +
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(50,100)) +
  theme(legend.position = "none")

ggarrange(p_Reff, p_infection, p_transmission,
          nrow = 1,
          widths = c(1, 1.15, 1.3), 
          align = "hv",
          labels = c(' a', ' b','   c'))

ggsave("supp_VE_heatmap1.pdf", device = cairo_pdf, width = 12, height = 4)


# _____________________________________________________________________
# Number infections averted ####
# _____________________________________________________________________
df$infections_averted_99 <- NA
df$infections_averted_50 <- NA

for (i in 1:dim(df)[1]){
  # computer number of infections without testing
  tot_inf <- compute_tot_infections(phi = this_phi, VE_I = df$VE_I[i], VE_S = df$VE_S[i], 
                                    theta = 0, q = 0, 
                                    psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                    H_I = this_H_I, H_S = this_H_S)
  
  df$infections_averted_99[i] <- tot_inf - compute_tot_infections(phi = this_phi, VE_I = df$VE_I[i], VE_S = df$VE_S[i], 
                                                                   theta = theta_99, q = 0, 
                                                                   psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                                                   H_I = this_H_I, H_S = this_H_S)
  
  df$infections_averted_50[i] <- tot_inf - compute_tot_infections(phi = this_phi, VE_I = df$VE_I[i], VE_S = df$VE_S[i], 
                                                                    theta = theta_50, q = 0, 
                                                                    psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                                                                    H_I = this_H_I, H_S = this_H_S)
  
  
}

p_99 <- ggplot(df, aes(x = VE_I*100, y = VE_S*100, fill = infections_averted_99)) + 
  geom_tile() +
  #stat_contour(aes(z = Reff_99), breaks = 1, size = 1) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("VE against infection (%)") +
  xlab("VE against transmission (%)") + 
  ggtitle("Infections averted due to testing") +
  labs(fill = "") + 
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,13000))

p_50 <- ggplot(df, aes(x = VE_I*100, y = VE_S*100, fill = infections_averted_50)) + 
  geom_tile() +
  #stat_contour(aes(z = Reff_50), breaks = 1, size = 1) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  ylab("VE against infection (%)") +
  xlab("VE against transmission (%)") + 
  ggtitle("Infections averted due to testing") +
  labs(fill = "") + 
  scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits=c(0,13000)) +
  theme(legend.position = "none")

ggarrange(p_50, p_99,
          ncol = 2,
          align = "hv")

ggsave("heatmaps3.pdf", device = cairo_pdf, width = 12, height = 4)

save(df, file="VE_supp_df.RData")




