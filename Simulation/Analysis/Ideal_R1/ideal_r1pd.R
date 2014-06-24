# rm(list=ls())

library(plyr)

# load data
load("~/amta-analytics/Simulation/all_r1.RData")

# clean data
source("clean.R")
all  <- clean(rand)

# Subset to only strength = 70
dat <- subset(all, str == 70)
dat$r1fin <- sapply(dat$r1pd, function(x) if(x > 0) 1 else if(x == 0) 0.5 else 0)

# T-Test
attach(dat)
t.test(r4rank[r1pd > 0 & type == 'amta'], r4rank[r1pd < 0 & type == 'amta'])
t.test(r4rank[r1pd > 0 & type == 'wpb'], r4rank[r1pd < 0 & type == 'wpb'])
detach(dat)

# Regression
x <- lm(r4rank ~ r1fin*type, dat)
summary(x)

##### Error by R1PD - 70 Only #####

ggplot(dat, aes(x = r1pd, y = r4rank, colour = type)) + 
  geom_smooth() + theme_bw()

dat2 <- ddply(dat, .(r1fin, type), 
              summarise, final= mean(r4rank))

ggplot(dat2, aes(x = factor(r1fin), y = final, fill = type)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  theme_bw()


##### Error by R1PD - All #####

all$err <- with(all, true_rank - r4rank)

all.sum <- ddply(all, .(true_rank, r1pd, type), summarise, 
                 err = mean(err))

p <- ggplot(all, aes(x = r1pd, y = err, fill = type, colour = type)) + 
  geom_line(stat = 'smooth') + 
  geom_point(aes(x = r1pd, y = err), data = all.sum, alpha = 0.7)
p + facet_wrap( ~ true_rank, nrow = 4) + geom_hline(yintercept=0) + theme_bw()




##### Other ######


#dat3 <- ddply(dat, .(r1fin, type), summarise,
#              r2 = mean(r2opp.str))

#ggplot(dat3, aes(x = factor(r1fin), y = r2, fill = type)) + 
#  geom_bar(stat="identity", position=position_dodge()) +
#  theme_bw()

useful <- c('r1pd', 'r2opp.str', 'r3opp.str', 'r4opp.str', 'type')

ggally_sm <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + stat_smooth(...)
  p$type <- "continuous"
  p$subType <- "sm"
  p
}

dat5 <- dat[ ,useful]

dat6 <- ddply(dat5, .(r1pd, type), summarise, r2opp.str = mean(r2opp.str),
              r3opp.str = mean(r3opp.str), r4opp.str =  mean(r4opp.str))

useful2 <- c('r1pd', 'r2rank', 'r3rank', 'r4rank', 'type')
dat7 <- dat[,useful2]

dat8 <- ddply(dat7, .(r1pd, type), summarise, r2rank = mean(r2rank),
              r3rank = mean(r3rank), r4rank =  mean(r4rank))

ggpairs(dat8, c(1,3,4,5), colour = 'type')

dat9 <- ddply(dat, .(r1pd, type), summarise, r2pd = mean(r2pd),
              r3pd = mean(r3pd), r4pd =  mean(r4pd))

ggpairs(dat9, c(1,3,4,5), colour = 'type')

##########

a1 <- subset(amta, team == 1)
a1 <- a1[ , c('r1opp.str', 'r1pd', 'r1round.bal', 'r4cum.bal', 'r4cum.cs', 'r4cum.pd', 'r4rank', 'true_rank', 'trial')]

t.test(a1$r4rank[a1$r1round.bal == 1], a1$r4rank[a1$r1round.bal == 0])

x <- lm(r4rank ~ r1round.bal, a1)
