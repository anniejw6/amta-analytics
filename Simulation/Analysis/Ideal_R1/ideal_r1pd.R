# rm(list=ls())

library(plyr)
library(ggplot2)

# load data
load("~/amta-analytics/Simulation/Analysis/all_r1.RData")

# clean data
source('~/amta-analytics/Simulation/Analysis/clean.R')
all  <- clean(pseud)

# Subset to only strength = 70
dat <- subset(all, str == 70)
dat$r1fin <- sapply(dat$r1pd, function(x) if(x > 0) 1 else if(x == 0) 0.5 else 0)

##### Error by R1PD - 70 Only #####

dat1 <- ddply(dat, .(r1pd, type), summarise, r4rank = mean(r4rank))
#ggplot(dat, aes(x = r1pd, y = r4rank, colour = type)) + 
#  geom_smooth() + theme_bw() + geom_point(data = dat1) + 
#  geom_hline(yintercept = 12.5)

ggplot(dat1, aes(x = r1pd, y = (12.5 - r4rank), fill = type)) + 
  geom_bar(position='dodge', stat = 'identity') + theme_bw() +
  ggtitle("Round 1 PD vs. Difference from True Rank")

##### Other ######
library(reshape)

dat.oppstr <- ddply(dat, .(r1pd, type), summarise, r2opp.str = mean(r2opp.str),
              r3opp.str = mean(r3opp.str), r4opp.str =  mean(r4opp.str))
dat.oppstr <- melt(dat.oppstr, id = c('r1pd', 'type'))

ggplot(dat.oppstr, aes(x = r1pd, y = (70 - value), fill = type, colour = type)) + 
  geom_bar(position='dodge', stat = 'identity', alpha = 0.7) + 
  facet_wrap( ~ variable)  + theme_bw()

dat.rank <- ddply(dat, .(r1pd, type), summarise, r2rank = mean(r2rank),
              r3rank = mean(r3rank), r4rank =  mean(r4rank))
dat.rank <- melt(dat.rank, id = c('r1pd', 'type'))

ggplot(dat.rank, aes(x = r1pd, y = (12.5 - value), fill = type, colour = type)) + 
  geom_bar(position='dodge', stat = 'identity', alpha = 0.7) + 
  facet_wrap( ~ variable)  + theme_bw()

########## Data Analysis #####

# T-Test
attach(dat)
t.test(r4rank[r1pd > 0 & type == 'amta'], r4rank[r1pd < 0 & type == 'amta'])
t.test(r4rank[r1pd > 0 & type == 'wpb'], r4rank[r1pd < 0 & type == 'wpb'])
detach(dat)

# Regression
x <- lm(r4rank ~ r1fin*type, dat)
summary(x)

##### Error by R1PD - All #####

all$err <- with(all, true_rank - r4rank)

all.sum <- ddply(all, .(true_rank, r1pd, type), summarise, 
                 err = mean(err))

p <- ggplot(all.sum, aes(x = r1pd, y = err, fill = type, colour = type)) + 
  geom_bar(position='dodge', stat = 'identity', alpha = 0.7) + theme_bw()
# geom_line(stat = 'smooth') + 
#  geom_point(aes(x = r1pd, y = err), data = all.sum, alpha = 0.7)

p + facet_wrap( ~ true_rank, nrow = 4) + geom_hline(yintercept=0) + theme_bw()
