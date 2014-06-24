rm(list=ls())

library(plyr)
library(ggplot2)

load("all_r1.RData")

source("clean.R")

d.envel <- clean(envel)
d.power <- clean(power)
d.fold  <- clean(fold)
d.norm  <- clean(rand)

x <- unique(c(colnames(d.envel), colnames(d.power),
              colnames(d.fold), colnames(d.norm)))
all <- rbind(d.envel[,x], d.power[,x], 
             d.fold[,x], d.norm[,x])

### Create Smaller Set #####
useful <- c('true_rank','r1opp.str', 'r2opp.str', 'r3opp.str', 
            'r4opp.str', 'r4rank', 'type', 'cat', 'trial')

all.sm <- all[ ,useful]
all.sm$mse <- with(all.sm, sqrt((r4rank - true_rank)^2))

all.sum <- ddply(all.sm, .(true_rank, type, cat), summarise,
                 r1opp.str = mean(r1opp.str),
                 r2opp.str = mean(r2opp.str),
                 r3opp.str = mean(r3opp.str), 
                 r4opp.str = mean(r4opp.str),
                 r4rank    = mean(r4rank),
                 mse       = mean(mse))

#### Calculate Mean Squared Error ####
sum.err <- ddply(all.sm, .(type, cat), summarise, mse = mean(mse))
mse.bar <- ggplot(sum.err, aes(x = type, y= mse, fill = cat)) + 
  geom_bar(stat= 'identity', position = 'dodge') + theme_bw()

print(mse.bar)
#ggsave(file="error_bar.png", plot = mse.bar, dpi = 200)

errors <- ggplot(all.sm, aes(x = true_rank, y= mse, fill = cat, colour = cat)) + 
  geom_line(stat = 'smooth') + 
  geom_point(data = all.sum, alpha = 0.5) + facet_grid(. ~ type) + 
  # geom_bar(stat= 'identity', position = 'dodge') + 
  theme_bw() 

print(errors)
#ggsave(file="error.png", plot = errors, dpi = 200)

##### Validate First Round #####
validate <- ggplot(all.sm, aes(x = true_rank, y = r1opp.str, 
                               colour = type, fill = type)) + 
  geom_line(stat = 'smooth') +
  geom_point(data = all.sum, alpha = 0.5) + 
  facet_grid(type ~ cat) + theme_bw()

print(validate)
#ggsave(file="validate.png", plot = validate, dpi = 200)
