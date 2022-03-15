# _____________________________________________________________________
# Import libraries ####
# _____________________________________________________________________
library(ggplot2)
library(deSolve)
library(reshape2)
library(plyr)
library(scico)
library(ggpubr)
library(grid)
library(pracma)
library(gridExtra)
library(scales)
library(RColorBrewer)
library(directlabels)
library(cetcolor)
library(viridis)
library(dplyr)
library(metR)
source("helpers.R")

## Run all these lines (14-18) if you haven't already installed fonts 
library(remotes)
#remotes::install_version("Rttf2pt1", version = "1.3.8")
library(extrafont)
# font_import()
loadfonts(device = "pdf", quiet = TRUE) # windows
# loadfonts(device = "pdf", quiet = TRUE) # mac
fonts()

# _____________________________________________________________________
# Setup colors and ggplot theme ####
# _____________________________________________________________________
mydarkteal <- "#205066" 
mylightteal <- "#82BCD6" 
mydarkorange <- "#C67100" 
mylightorange <- "#FFB95B"
mypurple <- "#950F73"
mygray <- "#636363"
mylightgray <- "#BDBDBD"
myblack <- "#252525" #really dark gray

mydarkgreen <- "#355D32"
mylightgreen <- "#7EB87A"

infcolor <- "#DE0D92"
transmcolor <- "#44AF69"
hospcolor <- "#360568"

theta99_purple <- "#CC069B"
theta50_purple <- "#8C126E"
thetabiwk_purple <- "#FF7ADE"

nolabels_theme <- theme(axis.title.x =element_blank(),
                        axis.text.x = element_blank(),
                        axis.title.y = element_blank(),
                        axis.text.y = element_blank(),
                        plot.title = element_text(face = "plain", family="Arial"),
                        legend.position = "none")
onlyx_theme <- theme(axis.title.y = element_blank(),
                     axis.text.y = element_blank(),
                     plot.title = element_text(face = "plain", family="Arial"),
                     legend.position = "none")
onlyy_theme <- theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     plot.title = element_text(face = "plain", family="Arial"),
                     legend.position = "none")
alllabels_theme <- theme(plot.title = element_text(face = "plain", family="Arial"),
                         legend.position = "none")

theme_set(theme_minimal(base_size = 12))
theme_update(text = element_text(family="Arial", size = 12),
             plot.title = element_text(size = 12, hjust = 0.5, family="Arial"))

my_linesize <- 1

# _____________________________________________________________________
# Model and population parameters ####
# _____________________________________________________________________
dt <- 1 
t <- seq(from=1, to=270, by=dt)

# Population size
N <- 20000 

# phi: proportion of population this is vaccinated
phi_vec <- seq(0, 1, by = 0.01)
this_phi <- 0.58 # 0.62  # fully vacc. in US as of 1/10
this_psi <- 0.35 # 0.50  # CDC estimate 

# Simulation parameters - time span and homophily
dt <- 1 
t <- seq(from=1, to=270, by=dt)
ext_forcing = 1 # imported cases

# Homophily parameter
this_q <- 0
q0 <- 0
qhigh <- 0.8

# _____________________________________________________________________
# Infection and immunity parameters ####
# _____________________________________________________________________
gamma <- 1/6 # 1/recovery period
sigma <- 1/3 # 1/latent period
ext_forcing <- 1 # ~ amount of daily imported cases

R0 <- 4
alpha <- R0*gamma/N # transmissibility
delta_X_S <- 0.63 # ref: Gardner, infection-acquired immunity effectiveness to decrease Susceptibility to infection
delta_X_I <- 0.13 # ref: Garnder, infection-acquired immunity effectiveness to decrease Infectiousness given infection
delta_X_P <- 0.54  # infection-acquired immunity effectiveness to decrease disease progression given infection

baseline_VE_S <- 0.65 # best guess from lots of sources including Gardner 
baseline_VE_I <- 0.35  # best guess from lots of sources including Eyre (UK) 
baseline_VE_P <- 0.86 # best guess 2/28

# Hybrid immunity
baseline_H_S <- (1-this_X_S)*baseline_VE_S + this_X_S  # 0.87 for baseline scenario
baseline_H_I <- (1-this_X_I)*baseline_VE_I + this_X_I  # 0.43 for baseline scenario
baseline_H_P <- max(baseline_VE_P, this_X_P) 

# set default parameters as the baseline scenario
this_X_S <- delta_X_S
this_X_I <- delta_X_I
this_X_P <- delta_X_P

this_VE_S <- baseline_VE_S
this_VE_I <- baseline_VE_I
this_VE_P <- baseline_VE_P
  
this_H_S <- baseline_H_S
this_H_I <- baseline_H_I
this_H_P <- baseline_H_P

# other VE scenarios: boosted and waning VE
boosted_VE_S <- 0.80
boosted_VE_I <- 0.60
boosted_VE_P <- 0.90 
boosted_H_S <- (1-this_X_S)*boosted_VE_S + this_X_S  # 0.87 for baseline scenario
boosted_H_I <- (1-this_X_I)*boosted_VE_I + this_X_I  # 0.43 for baseline scenario
boosted_H_P <- max(boosted_VE_P, this_X_P)
  
waning_VE_S <- 0.50
waning_VE_I <- 0.10
waning_VE_P <- 0.80 
waning_H_S <- (1-this_X_S)*waning_VE_S + this_X_S  # 0.87 for baseline scenario
waning_H_I <- (1-this_X_I)*waning_VE_I + this_X_I  # 0.43 for baseline scenario
waning_H_P <- max(waning_VE_P, this_X_P)

# Omicron-specific parameters
omicron_X_S <- 0.35 # Altarawneh. Gardner 2021 
omicron_X_I <- 0.05 # Altarawneh, Gardner 2021
omicron_X_P <- 0.74
  
omicron_VE_S <- 0.35 # Gardner 
omicron_VE_I <- 0.05 # Gardner
omicron_VE_P <- 0.77 # Best guess 2/28
omicron_H_S <- (1-omicron_X_S)*omicron_VE_S + omicron_X_S
omicron_H_I <- (1-omicron_X_I)*omicron_VE_I + omicron_X_I
omicron_H_P <- max(omicron_VE_P,omicron_X_P)

# Rate of hospitalization for unvaccinated, naive
infection_hosp_rate_delta <- 0.02 
infection_hosp_rate_omicron <- 0.01

# _____________________________________________________________________
# Testing parameters ####
# weekly testing, PCR, ref: Larremore 2021
# _____________________________________________________________________
theta_99 <- 0.473 # 99% compliance once weekly 
theta_50 <- 0.242 # 50% compliance once weekly
theta_99_biwk <- 0.808 # 99% compliance twice weekly

high_compliance <- 0.99
waning_compliance <- 0.50

this_freq <- 7 # weekly testing
