# rm(list=ls())
# setwd("~/Dropbox/Tabulation Sub-Committee/Simulation")
source('sim_func.R')

set.seed(628)

rand  <- sim(5000, 'random', sdev = 7)
power <- sim(5000, 'power')
fold  <- sim(5000, 'fold')
envel <- sim(5000, 'envelope')

rm(list=lsf.str())

save.image('all_r1.RData')