source("setup.R")

theme_set(theme_minimal(base_size = 11))
theme_update(text = element_text(family="Arial", size = 11),
             plot.title = element_text(size = 11, hjust = 0.5, family="Arial"))

# _____________________________________________________________________
# Supplementary Figure 6 : megaheatmap ####
# Same as Figure 5, but with:
# 1. % of infections averted testing everyone, 
# 2. % hospitalizations averted testing everyone, 
# 3. % of hospitalizations averted with unvax-only testing
#* I: Get data ####
# _____________________________________________________________________

# If you want to generate data yourself, start here! Otherwise, skip to line 169 to use pre-generated dataframes.

# run the model
ptm <- proc.time()

VE_list = c("baseline","waning","boosted")

for (v in VE_list){
  
  phi_vec <- seq(0, 1, by = 0.05) # fine grain: by = 0.01
  psi_vec <- seq(0, 1, by = 0.05)
  df <- expand.grid(phi = phi_vec, psi = psi_vec)
  
  df$Reff <- NA
  df$totinfections_notesting <- NA
  df$totinfections_99_everyone <- NA
  df$totinfections_50_everyone <- NA
  df$Reff_99_everyone <- NA
  df$Reff_50_everyone <- NA
  df$tothosp_notesting <- NA
  df$tothosp_99_everyone <- NA
  df$tothosp_50_everyone <- NA
  df$tothosp_99_uonly <- NA
  df$tothosp_50_uonly <- NA
  df$Reff_99_uonly <- NA
  df$Reff_50_uonly <- NA
  
  if(v == "baseline"){
    this_VE_I <- baseline_VE_I
    this_VE_S <- baseline_VE_S
    this_VE_P <- baseline_VE_P
    this_H_I  <- baseline_H_I
    this_H_S  <- baseline_H_S
    this_H_P  <- baseline_H_P
  }
  if(v == "waning"){
    this_VE_I <- waning_VE_I
    this_VE_S <- waning_VE_S
    this_VE_P <- waning_VE_P
    this_H_I  <- waning_H_I
    this_H_S  <- waning_H_S
    this_H_P  <- waning_H_P
  }
  if(v == "boosted"){
    this_VE_I <- boosted_VE_I
    this_VE_S <- boosted_VE_S
    this_VE_P <- boosted_VE_P
    this_H_I  <- boosted_H_I
    this_H_S  <- boosted_H_S
    this_H_P  <- boosted_H_P
  }
  
  for (i in 1:dim(df)[1]){
    
    testing_everyone <- 0 # if 0, just testing unvacc. when implementing testing
    df$Reff[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                               theta = 0, q = this_q,
                               df$psi[i], X_I = this_X_I, X_S = this_X_S,
                               H_I = this_H_I, H_S = this_H_S)
    
    df$totinfections_notesting[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                            theta = 0, q = this_q,
                                                            df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                            H_I = this_H_I, H_S = this_H_S) 
    
    df$tothosp_notesting[i] <- compute_tot_hospitalizations(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P, 
                                                            infection_hosp_rate = infection_hosp_rate_delta,
                                                            theta = 0, q = this_q, 
                                                            df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                            H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
    
    ## Unvaccinated only testing
    testing_everyone <- 0 # if 0, just testing unvacc. when implementing testing
    
    df$Reff_99_uonly[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                        theta = theta_99, q = this_q,
                                        df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                        H_I = this_H_I, H_S = this_H_S)
    df$Reff_50_uonly[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                        theta = theta_50, q = this_q,
                                        df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                        H_I = this_H_I, H_S = this_H_S)
    
    df$tothosp_99_uonly[i] <- compute_tot_hospitalizations(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P, 
                                                           infection_hosp_rate = infection_hosp_rate_delta,
                                                           theta = theta_99, q = this_q, 
                                                           df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                           H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
    df$tothosp_50_uonly[i] <- compute_tot_hospitalizations(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P, 
                                                           infection_hosp_rate = infection_hosp_rate_delta,
                                                           theta = theta_50, q = this_q, 
                                                           df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                           H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
    
    ## testing everyone
    testing_everyone <- 1 # if 0, just testing unvacc. when implementing testing
    
    df$Reff_99_everyone[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                           theta = theta_99, q = this_q,
                                           df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                           H_I = this_H_I, H_S = this_H_S)
    df$Reff_50_everyone[i] <- compute_Reff(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                           theta = theta_50, q = this_q,
                                           df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                           H_I = this_H_I, H_S = this_H_S)
    
    df$tothosp_99_everyone[i] <- compute_tot_hospitalizations(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P, 
                                                              infection_hosp_rate = infection_hosp_rate_delta,
                                                              theta = theta_99, q = this_q, 
                                                              df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                              H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
    df$tothosp_50_everyone[i] <- compute_tot_hospitalizations(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S, VE_P = this_VE_P, 
                                                              infection_hosp_rate = infection_hosp_rate_delta,
                                                              theta = theta_50, q = this_q, 
                                                              df$psi[i], X_I = this_X_I, X_S = this_X_S, X_P = this_X_P,
                                                              H_I = this_H_I, H_S = this_H_S, H_P = this_H_P)
    
    df$totinfections_99_everyone[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                              theta = theta_99, q = this_q,
                                                              df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                              H_I = this_H_I, H_S = this_H_S)
    df$totinfections_50_everyone[i] <- compute_tot_infections(df$phi[i], VE_I = this_VE_I, VE_S = this_VE_S,
                                                              theta = theta_50, q = this_q,
                                                              df$psi[i], X_I = this_X_I, X_S = this_X_S,
                                                              H_I = this_H_I, H_S = this_H_S)
  }
  
  df$percent_reduc_inf_99_testeveryone <- (df$totinfections_notesting - df$totinfections_99_everyone)/df$totinfections_notesting*100
  df$percent_reduc_inf_50_testeveryone <- (df$totinfections_notesting - df$totinfections_50_everyone)/df$totinfections_notesting*100
  df$percent_reduc_hosp_99_testeveryone <- (df$tothosp_notesting - df$tothosp_99_everyone)/df$tothosp_notesting*100
  df$percent_reduc_hosp_50_testeveryone <- (df$tothosp_notesting - df$tothosp_50_everyone)/df$tothosp_notesting*100
  df$percent_reduc_hosp_99_uonly <- (df$tothosp_notesting - df$tothosp_99_uonly)/df$tothosp_notesting*100
  df$percent_reduc_hosp_50_uonly <- (df$tothosp_notesting - df$tothosp_50_uonly)/df$tothosp_notesting*100
  
  
  
  if(v == "baseline"){
    baselinedf <- df
  }
  if(v == "waning"){
    waningdf <- df
  }
  if(v == "boosted"){
    boosteddf <- df
  }
  
}


proc.time() - ptm
#saveRDS(df,file="df_suppFig5_boosted_R04_testeveryone.RData")


#* Dataframes for supplementary figure 6 - megaheatmap  ####
waningdf <- readRDS("dataframes/df_suppFig5_waning_R04_testeveryone_hosp.RData")
baselinedf <- readRDS("dataframes/df_suppFig5_baseline_R04_testeveryone_hosp.RData")
boosteddf <- readRDS("dataframes/df_suppFig5_boosted_R04_testeveryone_hosp.RData")

megafig_scenarios = c("hosp_testeveryone","hosp_uonly","inf_testeveryone")

for (m in megafig_scenarios){
  
  base <- "percent_reduc_"
  if (m == "hosp_testeveryone"){ 
    fill_50 <- paste0(base,"hosp_50_testeveryone")
    fill_99 <- paste0(base,"hosp_99_testeveryone")
    Reff_50 <- "Reff_50_everyone"
    Reff_99 <- "Reff_99_everyone"
  }
  if (m == "hosp_uonly"){ 
    fill_50 <-"percent_reduc_hosp_50_uonly"
    fill_99 <- "percent_reduc_hosp_99_uonly"
    Reff_50 <- "Reff_50_uonly"
    Reff_99 <- "Reff_99_uonly"
  }
  if (m == "inf_testeveryone"){ 
    fill_50 <- paste0(base,"inf_50_testeveryone")
    fill_99 <- paste0(base,"inf_99_testeveryone")
    Reff_50 <- "Reff_50_everyone"
    Reff_99 <- "Reff_99_everyone"
  }
  
  #* II: Plot suppfig5 ####
  for (i in 1:3) {
    if (i == 1) {
      df <- waningdf
    } else if (i==2) {
      df <- baselinedf
    } else {
      df <- boosteddf
    }
    
    percentreduc50 <- ggplot(df, aes(x = phi*100, y = psi*100)) + #, colour = ..level..)) +
      geom_raster(aes_string(fill = fill_50)) +
      geom_contour(aes_string(z = Reff_50), breaks = 1, size = 0.6, color = "white") +
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
      geom_raster(aes_string(fill = fill_99)) +
      geom_contour(aes_string(z = Reff_99), breaks = 1, size = 0.6, col = "white") +
      geom_contour(aes(z = Reff), breaks = 1, size = 0.6, col = "white", linetype = "longdash") +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(expand = c(0, 0)) +
      ylab("Infection-acquired immunity (%)") +
      xlab("") +# xlab("Population vaccination rate (%)") +
      #ggtitle("Weekly testing, 99% compliance", "% reduction in infections due to testing") +
      scale_fill_gradientn(colours = cet_pal(5, name = "inferno"), limits = c(0, 100)) +
      coord_fixed(1) +
      labs(fill = "") +
      theme(axis.title.y = element_blank(),
            plot.title = element_blank(),
            plot.subtitle = element_blank()) #element_text(hjust = 0.5))
    
    percent_legend <- get_legend(percentreduc99)
    
    if (i == 1){
      percentreduc50_waning <- percentreduc50 + onlyy_theme 
      percentreduc99_waning <- percentreduc99 + theme(legend.position = "none", plot.title = element_blank())
      if (m == "inf_testeveryone") { 
        percentreduc50_waning <- percentreduc50_waning + ggtitle("Waning/low VE") }
      if (m == "hosp_uonly") { 
        percentreduc99_waning <- percentreduc99_waning + alllabels_theme }
      else { percentreduc99_waning <- percentreduc99_waning + onlyy_theme }
      
    } else if (i == 2){
      percentreduc50_baseline <- percentreduc50 + nolabels_theme 
      percentreduc99_baseline <- percentreduc99 + theme(legend.position = "none", plot.title = element_blank())
      if (m == "inf_testeveryone") { 
        percentreduc50_baseline <- percentreduc50_baseline + ggtitle("Baseline VE") }
      if (m == "hosp_uonly") { 
        percentreduc99_baseline <- percentreduc99_baseline + onlyx_theme +  xlab("Population vaccination rate (%)") }
      else { percentreduc99_baseline <- percentreduc99_baseline + nolabels_theme }
    } else {
      percentreduc50_boosted <- percentreduc50 + nolabels_theme 
      percentreduc99_boosted <- percentreduc99 + theme(legend.position = "none", plot.title = element_blank())
      if (m == "inf_testeveryone") { 
        percentreduc50_boosted <- percentreduc50_boosted + ggtitle("Boosted/high VE") }
      if (m == "hosp_uonly") { 
        percentreduc99_boosted <- percentreduc99_boosted + onlyx_theme }
      else { percentreduc99_boosted <- percentreduc99_boosted + nolabels_theme }
    } 
  }
  
  if (m == "inf_testeveryone"){ 
    inf_everyone_panel <- ggarrange(percentreduc50_waning, NULL, percentreduc50_baseline, NULL, percentreduc50_boosted,
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
  }
  if (m == "hosp_testeveryone"){ 
    hosp_everyone_panel <- ggarrange(percentreduc50_waning, NULL, percentreduc50_baseline, NULL, percentreduc50_boosted,
                                     NULL, NULL, NULL, NULL, NULL,
                                     percentreduc99_waning, NULL, percentreduc99_baseline, NULL, percentreduc99_boosted,
                                     nrow = 3, ncol = 5,
                                     align = "hv",
                                     widths = c(1, -0.14, 1, -0.14, 1),
                                     heights = c(1, -0.22, 1),
                                     labels = c("g", NA, "    h", NA,"    i",
                                                NA, NA, NA, NA, NA,
                                                "j", NA, "    k", NA, "    l"),
                                     label.y = 0.88)
  }
  if (m == "hosp_uonly"){ 
    hosp_uonly_panel <-ggarrange(percentreduc50_waning, NULL, percentreduc50_baseline, NULL, percentreduc50_boosted,
                                 NULL, NULL, NULL, NULL, NULL,
                                 percentreduc99_waning, NULL, percentreduc99_baseline, NULL, percentreduc99_boosted,
                                 nrow = 3, ncol = 5,
                                 align = "hv",
                                 widths = c(1, -0.14, 1, -0.14, 1),
                                 heights = c(1, -0.22, 1),
                                 labels = c("m", NA, "    n", NA,"    o",
                                            NA, NA, NA, NA, NA,
                                            "p", NA, "    q", NA, "    r"),
                                 label.y = 0.88)
  }
  
  
}


lay <- rbind(c(rep(1,10),NA,NA),
             c(rep(2,10),3,NA),
             c(rep(4,10),NA,NA))

suppfig5 <- grid.arrange(inf_everyone_panel, hosp_everyone_panel, percent_legend, hosp_uonly_panel, 
                         layout_matrix = lay,
                         heights = c(1,1,1.15),
                         left = c("Infection-acquired immunity (%)"))

ggsave("suppFig5_megafig.pdf", suppfig5, device = cairo_pdf, width = 6, height = 10)
ggsave("suppFig5_megafig.svg", suppfig5, device = svg, width = 8, height = 12)
