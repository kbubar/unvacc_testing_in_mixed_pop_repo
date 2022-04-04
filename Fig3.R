source("setup.R")

# _____________________________________________________________________
# FIGURE 3: ####
# Reff and transition lines for the four scenarios: waning, baseline and boosted VE vs delta, plausible omicron
#
#*
#* I: Get data ####
#* If you want to generate data yourself, run the code in Fig2.R. Otherwise, start here to use pre-generated dataframes.
# _____________________________________________________________________

# Uses the same dataframe as figure 2
baselinedf <- readRDS("dataframes/df_Fig2_baseline.RData")
waningdf <- readRDS("dataframes/df_Fig2_waning.RData")
boosteddf <- readRDS("dataframes/df_Fig2_boosted.RData")
omicrondf <- readRDS("dataframes/df_Fig2_omicron.RData")

# _____________________________________________________________________
#* II: Plot fig3 ####
# _____________________________________________________________________

p_waning <- ggplot(waningdf, aes(x = phi*100, y = psi*100)) +
  geom_contour(aes(z = dom_transmission), breaks = 50, size = my_linesize, color = transmcolor) +
  geom_contour(aes(z = breakthrough), breaks = 50, size = my_linesize, color = infcolor) +
  geom_contour(aes(z = breakthrough_hosp), breaks = 50, size = my_linesize, color = hospcolor) +
  geom_contour(aes(z = Reff),breaks = 1, size = my_linesize/2, color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("Infection-acquired immunity (%)") +
  xlab("Population vaccination rate (%)") +
  ggtitle("Waning/low VE") +
  coord_fixed(1) +
  theme(legend.position = "none")

p_baseline <- ggplot(baselinedf, aes(x = phi*100, y = psi*100)) +
  geom_contour(aes(z = dom_transmission), breaks = 50, size = my_linesize, color = transmcolor) +
  geom_contour(aes(z = breakthrough), breaks = 50, size = my_linesize, color = infcolor) +
  geom_contour(aes(z = breakthrough_hosp), breaks = 50, size = my_linesize, color = hospcolor) +
  geom_contour(aes(z = Reff),breaks = 1, size = my_linesize/2, color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("") +
  xlab("Population vaccination rate (%)") +
  ggtitle("Baseline VE") +
  coord_fixed(1) +
  theme(legend.position = "none",
        axis.title.y = element_blank())

p_boosted <- ggplot(boosteddf, aes(x = phi*100, y = psi*100)) +
  geom_contour(aes(z = dom_transmission), breaks = 50, size = my_linesize, color = transmcolor) +
  geom_contour(aes(z = breakthrough), breaks = 50, size = my_linesize, color = infcolor) +
  geom_contour(aes(z = breakthrough_hosp), breaks = 50, size = my_linesize, color = hospcolor) +
  geom_contour(aes(z = Reff),breaks = 1, size = my_linesize/2, color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("") +
  xlab("Population vaccination rate (%)") +
  ggtitle("Boosted/high VE") +
  coord_fixed(1) +
  theme(legend.position = "none",
        axis.title.y = element_blank())

p_omicron <- ggplot(omicrondf, aes(x = phi*100, y = psi*100)) +
  geom_contour(aes(z = dom_transmission), breaks = 50, size = my_linesize, color = transmcolor) +
  geom_contour(aes(z = breakthrough), breaks = 50, size = my_linesize, color = infcolor) +
  geom_contour(aes(z = breakthrough_hosp), breaks = 50, size = my_linesize, color = hospcolor) +
  geom_contour(aes(z = Reff),breaks = 1, size = my_linesize/2, color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("") +
  xlab("Population vaccination rate (%)") +
  ggtitle("Plausible Omicron") +
  coord_fixed(1) +
  theme(legend.position = "none",
        axis.title.y = element_blank())

fig3 <- ggarrange(p_waning, p_baseline, p_boosted, p_omicron,NULL,
                  nrow = 1,
                  align = "hv",
                  labels = c("a", "b", "c", "d",NULL),
                  widths = c(1,1,1,1,0.1))
plot(fig3)

ggsave("Fig3.pdf", fig3, device = cairo_pdf, width = 10, height = 3)
ggsave("Fig3.svg", fig3, device = svg, width = 10, height = 3)
