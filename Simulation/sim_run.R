# rm(list=ls())
# setwd("~/Dropbox/Tabulation Sub-Committee/Simulation")
source('sim_func.R')

set.seed(628)

#rand  <- sim(5000, 'random', sdev = 7)
power <- sim(5000, 'power')
fold  <- sim(5000, 'fold')
envel <- sim(5000, 'envelope')
pseud <- sim(5000, 'pseudo-rand')

rm(list=lsf.str())

#save.image('all_r1.RData')
save.image("~/Dropbox/all_R1.RData")