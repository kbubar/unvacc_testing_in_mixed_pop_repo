# _____________________________________________________________________
# HOW TO RUN THE MODEL ####
# Always start by running setup.R to 
#   initialize default parameter values
#   sources relevant packages
#   run helpers.R which includes all model and plotting functions
# _____________________________________________________________________
source("setup.R") 

# To run the model, call run_leaky_model
# Inputs:
#   phi = fraction of population vaccinated
#   VE_I = vaccine effectiveness to decrease infectiousness given infection
#   VE_S = vaccine effectiveness to decrease susceptibility to infection
#   theta = fraction by which screening & isolation reduces typical infectious period (0 if not testing)
#   q = degree of social mixing, not included in final manuscript (ALWAYS SET THIS TO 0)
#   psi = fraction of population with prior infection-acquired immunity
#   X_I = infection-acquired effectiveness to decrease infectiousness given infection (X for experienced)
#   X_S = infection-acquired effectiveness to decrease susceptibility to infection
#   H_I = hybrid-immunity effectiveness to decrease infectiousness given infection
#   H_S = hybrid-immunity effectiveness to decrease susceptibility to infection
# 
# Output: 
#   dataframe of model compartments over time

this_theta <- 0
this_q <- 0
df <- run_leaky_model(this_phi, this_VE_I, this_VE_S, this_theta, this_q,
                      psi = this_psi, X_I = this_X_I, X_S = this_X_S,
                      H_I = this_H_I, H_S = this_H_S)

# Plot number infected over time
ggplot(df, aes(x = time)) +
  geom_line(aes(y = I_v + I_h), col = mylightgray, size = my_linesize) +
  geom_line(aes(y = I_u + I_x), col = mygray, size = my_linesize, linetype = "longdash")  +
  geom_line(aes(y = I_v + I_u + I_x + I_h), col = myblack, size = my_linesize) +
  ylab("Infected (#)") +
  xlab("Time (days)") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 200)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) + # 2500 for R0 = 6
  alllabels_theme

# To compute number of hospitalizations, run compute_new_daily_hosp
# Inputs:
#   Same as above + effectiveness against disease progression for VE, XE and HE
# Output:
#   list of hospitalizations over time by vaccination status ([[1]] = unvaccinated, [[2]] = vaccinated)

list_hosp <- compute_new_daily_hosp(this_phi, this_VE_I, this_VE_S, this_VE_P,
                                    this_theta, this_q,
                                    this_psi, this_X_I, this_X_S, this_X_P,
                                    this_H_I, this_H_S, this_H_P)

df$hosp_u <- unlist(list_hosp[[1]])
df$hosp_v <- unlist(list_hosp[[2]])

ggplot(df, aes(x = time)) +
  geom_line(aes(y = hosp_v), col = mylightgray, size = my_linesize) +
  geom_line(aes(y = hosp_u), col = mygray, size = my_linesize, linetype = "longdash")  +
  geom_line(aes(y = hosp_u + hosp_v), col = myblack, size = my_linesize) +
  ylab("New daily hosp. (#)") +
  xlab("Time (days)") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 200)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,2)) + # 2500 for R0 = 6
  alllabels_theme

# To recreate plots from the manuscripts, see Fig and SuppFig scripts