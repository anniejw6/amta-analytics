# rm(list=ls())
setwd("~/Dropbox/Tabulation Sub-Committee/Simulation")
source('sim_func.R')

# Simulate two tournaments with the same set of teams and Round 1 assignments
# Then, pair rounds 2-4 using AMTA and WPB

x <- sim(1, 'random')
y <- sim(1, 'power')
z <- sim(1, 'fold')
a <- sim(1, 'envelope')
