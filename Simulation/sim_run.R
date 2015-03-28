
# setwd("~/Dropbox/Tabulation Sub-Committee/Simulation")
# rm(list=ls())
source('sim_func.R')
source('pair_func.R')
source('wrapper.R')
source('power_protect.R')

set.seed(628)

#str <- c(81:71, 70, 70, 69:59)

str <- c(120:110, 70, 70, 69:59)

x <- sim(100, type = 'random', str = str, pp = T, wpb_opt = F)

set.seed(628)

y <- sim(100, type = 'random', str = str, pp = F, wpb_opt = F)

pp1 <- x$amta
pp0 <- y$amta

#pseud <- sim(5000, 'pseudo-rand', str = str)
#power <- sim(5000, 'power', str = str)
#fold  <- sim(5000, 'fold',  str = str)
#envel <- sim(5000, 'envelope', str = str)


#rm(list=lsf.str())

#save.image('all_r1.RData')


q1 <- ddply(pp1,.(trial), summarize, c = sum(true_rank <= 6 & r4rank <= 6))
q0 <- ddply(pp0,.(trial), summarize, c = sum(true_rank <= 6 & r4rank <= 6))

mean(q1$c)
mean(q0$c)