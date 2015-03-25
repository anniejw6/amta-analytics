
# setwd("~/Dropbox/Tabulation Sub-Committee/Simulation")
# rm(list=ls())
source('sim_func.R')
source('../Pairing/pair_func.R')

set.seed(628)

str <- c(81:71, 70, 70, 69:59)

str <- c(120:110, 70, 70, 69:59)
#rand  <- sim(5000, 'random', sdev = 7)
pseud <- sim(5000, 'pseudo-rand', str = str)
power <- sim(5000, 'power', str = str)
fold  <- sim(5000, 'fold',  str = str)
envel <- sim(5000, 'envelope', str = str)


rm(list=lsf.str())

#save.image('all_r1.RData')
save.image("all_R1.RData")