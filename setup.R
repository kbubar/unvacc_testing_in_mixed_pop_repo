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

## Run all these lines (14-18) if you haven't already installed fonts 
library(remotes)
remotes::install_version("Rttf2pt1", version = "1.3.8")
library(extrafont)
# font_import()
#loadfonts(device = "win", quiet = TRUE)
# fonts()

source("helpers.R")

# SETUP ####
mydarkteal <- "#205066" #"#27627B"
mylightteal <- "#82BCD6" #"#3B94BA"
mydarkorange <- "#C67100" #D07600"
mylightorange <- "#FFB95B" #FF9E1F"
mypurple <- "#950F73"
mygray <- "#636363"
mylightgray <- "#BDBDBD"
myblack <- "#252525" #really dark gray

mygreen <- "#2A6543"
myred <- "#8B0000"
myblue <- "#77C3E4"
myyellow <- "#EAC435"

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

theme_set(theme_minimal(base_size = 14))
theme_update(text = element_text(family="Arial"),
             plot.title = element_text(size = 14, hjust = 0.5, family="Arial"))

my_linesize <- 1

scientific <- function(x){
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scientific_format()(x)))))
}

###########
# Pop parameters
N <- 20000

phi_vec <- seq(0, 1, by = 0.01)
this_phi <- 0.58  # fully vacc. in US as of 11/4

this_psi <- 0.35  # CDC estimate 
this_X_S <- 0.627 # Gardner 2021 
this_X_I <- 0.125 # Gardner 2021

this_H_S <- 0.827 # Gardner 2021
this_H_I <- 0.464 # Gardner 2021

# Simulation parameters - time span and homophily
dt <- 1 
t <- seq(from=1, to=270, by=dt)
ext_forcing = 1 # imported cases

this_q <- 0
q0 <- 0
qhigh <- 0.8

# Disease parameters
gamma <- 1/6 # 1/recovery period
sigma <- 1/3 # 1/latent period

R0 <- 4
alpha <- R0*gamma/N # transmissibility
this_VE_S <- 0.70 # best guess from lots of sources including Gardner 
this_VE_I <- 0.20 # best guess from lots of sources including Gardner 

# Testing parameters
ideal_theta <- 0.808 # ideal testing: twice weekly, PCR, 99% compliance
mod_theta <- 0.473 # moderate testing: weekly, PCR, 99% compliance
real_theta <- 0.242 # realistic testing: weekly, PCR, 50% compliance

high_compliance <- 0.99
low_compliance <- 0.5

high_freq <- 3.5
low_freq <- 7
