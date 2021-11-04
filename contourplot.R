# TEST of contour plot
library(directlabels)
phi_vec <- seq(0, 1, by = 0.1)
psi_vec <- seq(0, 1, by = 0.1)

df <- expand.grid(phi = phi_vec, psi = psi_vec)
df$Reff <- NA

for (i in 1:dim(df)[1]){
  df$Reff[i] <- compute_Reff(df$phi[i], VE_S = 0.8, VE_I = 0.62, theta = 0, q = 0, 
                             df$psi[i], X_I = 0.62, X_S = 0.8)
}

g <- ggplot(df, aes(x = phi*100, y = psi*100, z = Reff, colour = ..level..)) + 
  stat_contour(breaks = 1:R0, size = 1) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  ylab("psi") +
  xlab("phi") + 
  ggtitle(expression(R[eff]))

direct.label(g, list(last.points, hjust = 1, vjust = -2))
