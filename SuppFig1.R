source("setup.R")

theme_set(theme_minimal(base_size = 11))
theme_update(text = element_text(family="Arial", size = 11),
             plot.title = element_text(size = 11, hjust = 0.5, family="Arial"))

# _____________________________________________________________________
# SUPP FIG1 - heatmaps of total infections and Reff contours, R0 = 4,6 ####
# _____________________________________________________________________

# To run data, See Figure 2

#  Read in data   ####
for (thisrow in 1:2){
  if (thisrow == 1){
    baselinedf <- readRDS("dataframes/df_fig2_baseline.RData")
    waningdf <- readRDS("dataframes/df_fig2_waning.RData")
    boosteddf <- readRDS("dataframes/df_fig2_boosted.RData")
  } else {
    baselinedf <- readRDS("dataframes/df_suppfig2_baseline_R06.RData")
    waningdf <- readRDS("dataframes/df_suppfig2_waning_R06.RData")
    boosteddf <- readRDS("dataframes/df_suppfig2_boosted_R06.RData")
  }
  
#  Plot Supp Fig 1  ####
  p_waning <- ggplot(waningdf, aes(x = phi*100, y = psi*100, z = Reff))+ 
    geom_raster(aes(fill = tot_infections)) +
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
    geom_raster(aes(fill = tot_infections)) +
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
    geom_raster(aes(fill = tot_infections)) +
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

suppfig1 <- arrangeGrob(panels, num_legend, layout_matrix = lay,
                        widths = c(3, 0.5), 
                        left = c("Infection-acquired immunity (%)"))

ggsave("SuppFig1.pdf", suppfig1, device = cairo_pdf, width = 8, height = 5)
ggsave("SuppFig1.svg", suppfig1, device = svg, width = 8, height = 5)