# rm(list=ls())
# setwd("~/Dropbox/Tabulation Sub-Committee/Simulation")
source('sim_func.R')

set.seed(628)

str <- c(81:71, 70, 70, 69:59)

#rand  <- sim(5000, 'random', sdev = 7)
power <- sim(5000, 'power', str = sample(str))
fold  <- sim(5000, 'fold', str = sample(str))
envel <- sim(5000, 'envelope', str = sample(str))
pseud <- sim(5000, 'pseudo-rand', str = sample(str))

rm(list=lsf.str())

#save.image('all_r1.RData')
save.image("all_R1.RData")