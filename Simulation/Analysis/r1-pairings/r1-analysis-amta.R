rm(list=ls())

library(plyr)
library(ggplot2)

load("~/amta-analytics/Simulation/Analysis/all_r1.RData")
setwd("~/amta-analytics/Simulation/Analysis/r1-pairings")

envel <- envel$amta
power <- power$amta
fold  <- fold$amta
norm  <- pseud$amta

all <- rbind(envel, power, fold, norm)

##### Data Cleaning #####

# Recode category
all$cat <- sapply(all$cat, function(x){
  if(x == 'envelope') return("Envelope")
  else if(x == 'fold') return("Folding")
  else if(x == 'power') return("Power-Matching")
  else if(x == 'pseudo-rand') return("Random")
  else return('HELP')
})


# Add Error Values
all$bias <- with(all, r4rank - true_rank)
all$sqerr <- with(all, bias^2)
all$sos <- with(all, (r1opp.str + r2opp.str + r3opp.str + r4opp.str)/4)

##### Validate First Round #####

## Get data structure
processR1 <- function(dat, category, seed = 628){
  set.seed(628)
  df <- subset(dat[dat$trial == sample(1:max(dat$trial), 1), ], cat == category)
  df <- df[, c('str', 'r1opp.str')]
  df$str <- rank(-df$str)
  df$r1opp.str <- rank(-df$r1opp.str)
  df <- stack(df)
  df$courtroom <- LETTERS[1:24]
  return(df)
}

for(i in unique(all$cat)){
  
  # Subset data frame and clean
  plot <- ggplot(data = processR1(dat = all, category = i), 
                 aes(x = factor(ind, levels = c('str', 'r1opp.str')), 
                     y = factor(values, levels = rev(unique(df$values))), 
                     group = courtroom, colour = courtroom, 
                     label = values)) + 
    geom_line() + geom_point() +  
    scale_x_discrete(labels = c("Team\nStrength", "Opponent\nStrength"),
                     expand=c(0.15,0)) + 
    # geom_text(data = df[df$ind == "str", ], hjust=1.1, vjust=0.4) +
    #  geom_text(data = df[df$ind == "r1opp.str", ], hjust=-0.1, vjust=0.4) +
    ggtitle(i) + ylab('True Rank') +
    theme_bw() + theme(legend.position="none", axis.title.x = element_blank(), 
                       axis.text = element_text(size = 8, color = 'grey', family = 'Palatino'),
                       axis.title.y = element_text(size = 8, color = 'grey', family = 'Palatino'),
                       axis.title.x = element_blank(),
                       plot.title = element_text(size= 14, vjust=1.25, family = 'Palatino'),
                       panel.grid.minor.x=element_blank(), 
                       panel.grid.major.x=element_blank(),
                       axis.ticks = element_blank())
  
  ggsave(filename = tolower(paste0('graphs/',i, '_demo.pdf')), 
         plot = plot, width = 5, height = 5)
}


#Validate Plots
#validate <- ggplot(all, aes(x = true_rank, y = r1opp.str)) + 
#  geom_line(stat = 'smooth') +
#  geom_point(data = all.sum, alpha = 0.5) + 
#  facet_grid( ~ cat) + theme_bw()

#print(validate)
#ggsave(file="validate.png", plot = validate, dpi = 200)

#### Bias-Variance ####
sum.err <- ddply(all, .(trial, cat), summarise, 
                 bias = mean(bias),
                 variance = mean(sqerr),
                 rmse = sqrt(mean(sqerr)))

sum.err <- ddply(sum.err, .(cat), summarise, 
                 bias = mean(bias),
                 variance = mean(variance),
                 rmse = mean(rmse))

mse.bar <- ggplot(sum.err, aes(x = factor(cat, 
                                          levels = c('Random', 'Envelope', 'Folding', 'Power-Matching')), 
                               y = bias, 
                               ymin = bias,
                               ymax = bias + rmse,
                               colour = cat)) + 
  #geom_bar(stat = 'identity') +
  geom_linerange(size = 2, alpha = 0.4) + 
  geom_point(size = 5) + 
  theme_bw() + geom_text(aes(y = bias + rmse + 0.1, label = round(rmse,2)), 
                         colour = 'black', 
                         size = 4,
                         family = 'Palatino') +
  guides(colour = F) + ylab('Error') + ggtitle("Bias and Variance") +
  theme(legend.position="none", 
        axis.title.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, family = 'Palatino'),
        axis.title.y = element_text(size = 10, family = 'Palatino'),
        axis.title.x = element_blank(),
        plot.title = element_text(size= 14, vjust=1.25, family = 'Palatino'),
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank(),
        axis.ticks = element_blank())

ggsave(file="graphs/error_bar.pdf", plot = mse.bar, width = 5, height = 4)

#errors <- ggplot(all, aes(x = true_rank, 
#                          y = mse, 
#                          fill = cat, 
#                          colour = cat)) + 
#  geom_line(stat = 'smooth') + 
#  geom_point(data = all.sum, alpha = 0.5) + 
# geom_bar(stat= 'identity', position = 'dodge') + 
#  theme_bw()

#print(errors)
#ggsave(file="error.png", plot = errors, dpi = 200)

### Team Rank Error Graph ####

# Add Summary
all.sum <- ddply(all, .(true_rank, cat), summarise,
                 r1opp.str = mean(r1opp.str),
                 r2opp.str = mean(r2opp.str),
                 r3opp.str = mean(r3opp.str), 
                 r4opp.str = mean(r4opp.str),
                 r4rank    = mean(r4rank),
                 rmse      = sqrt(mean(sqerr)),
                 bias      = mean(bias),
                 str       = mean(str),
                 sos       = mean(sos))

all.sum$cat <- factor(all.sum$cat, 
                      levels = c('Random', 'Envelope', 'Folding', 'Power-Matching'))

plot <- ggplot(all.sum, aes(x = true_rank, y = bias, 
                            fill = cat, colour = cat)) + 
  # geom_smooth() + 
  geom_bar(stat= 'identity', alpha = 0.5, position = 'dodge') + 
  theme_bw() + facet_grid(~cat) +
  #ggtitle("Bias by Team Strength") + 
  ylab('Bias') + xlab('True Rank') +
  theme(legend.position="none", 
        #axis.title.x = element_blank(), 
        #axis.text.y = element_blank(),
        axis.text = element_text(size = 10, family = 'Palatino'),
        axis.title = element_text(size = 10, family = 'Palatino'),
        strip.text.x = element_text(size = 10, family = 'Palatino'),
        plot.title = element_text(size= 14, vjust=1.25, family = 'Palatino'),
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank(),
        axis.ticks = element_blank())

ggsave(file="graphs/bias_teamRank.pdf", plot = plot, width = 8.5, height = 2)

plot <- ggplot(all.sum, aes(x = true_rank, y = rmse, 
                            fill = cat, colour = cat)) + 
  # geom_smooth() + 
  geom_bar(stat= 'identity', alpha = 0.5, position = 'dodge') + 
  theme_bw() + facet_grid(~cat) +
 # ggtitle("Variance by Team Strength") + 
  ylab('Root Mean Squared Error') + xlab('True Rank') +
  theme(legend.position="none", 
        #axis.title.x = element_blank(), 
        #axis.text.y = element_blank(),
        axis.text = element_text(size = 10, family = 'Palatino'),
        axis.title = element_text(size = 10, family = 'Palatino'),
        strip.text.x = element_text(size = 10, family = 'Palatino'),
        plot.title = element_text(size= 14, vjust=1.25, family = 'Palatino'),
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank(),
        axis.ticks = element_blank())

ggsave(file="graphs/rmse_teamRank.pdf", plot = plot, width = 8.5, height = 2)

ggplot(subset(all.sum, cat %in% c('Folding', 'Random')), 
       aes(x = true_rank, y = bias, 
                    fill = cat, colour = cat)) + 
  # geom_smooth() + 
  geom_bar(stat= 'identity', alpha = 0.5, position = 'dodge') + 
  theme_bw() +
  #ggtitle("Bias by Team Strength") + 
  ylab('Bias') + xlab('True Rank') +
  theme(#legend.position="none", 
        #axis.title.x = element_blank(), 
        #axis.text.y = element_blank(),
        axis.text = element_text(size = 10, family = 'Palatino'),
        axis.title = element_text(size = 10, family = 'Palatino'),
        strip.text.x = element_text(size = 10, family = 'Palatino'),
        plot.title = element_text(size= 14, vjust=1.25, family = 'Palatino'),
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank(),
        axis.ticks = element_blank())

ggsave(file="graphs/bias_ranfold.pdf", width = 5, height = 3.5)

ggplot(subset(all.sum, cat %in% c('Folding', 'Random')), 
       aes(x = true_rank, y = rmse, 
           fill = cat, colour = cat)) + 
  # geom_smooth() + 
  geom_bar(stat= 'identity', alpha = 0.5, position = 'dodge') + 
  theme_bw() +
  #ggtitle("Variance by Team Strength") + 
  ylab('Root Mean Squared Error') + xlab('True Rank') +
  theme(#legend.position="none", 
    #axis.title.x = element_blank(), 
    #axis.text.y = element_blank(),
    axis.text = element_text(size = 10, family = 'Palatino'),
    axis.title = element_text(size = 10, family = 'Palatino'),
    strip.text.x = element_text(size = 10, family = 'Palatino'),
    plot.title = element_text(size= 14, vjust=1.25, family = 'Palatino'),
    panel.grid.minor.x=element_blank(), 
    panel.grid.major.x=element_blank(),
    axis.ticks = element_blank())

ggsave(file="graphs/rmse_ranfold.pdf", width = 5, height = 3.5)

x <- ddply(all.sum, .(true_rank), summarise, 
           bias.diff = bias[cat == 'Random'] - bias[cat == 'Folding'],
           rmse.diff = rmse[cat == 'Random'] - rmse[cat == 'Folding'])
sum(x[,2] < 0)
sum(x[,3] < 0)

##### Other Crap #####

#### Strength of Schedule within a team #### 
ggplot(all.sum, aes(x = true_rank, y = sos - str, fill = cat, colour = cat)) + 
  geom_bar(stat= 'identity', position = 'dodge') + facet_grid(cat ~ type) + 
  theme_bw() + theme(legend.position="none")

ggplot(all.sum, aes(x = sqrt((sos - str)^2), y = mse, fill = cat, colour = cat)) + 
  geom_point() + stat_smooth(data = all) + facet_grid(cat ~ type) + 
  theme_bw() + theme(legend.position="none")

ggplot(all.sum, aes(x = sos - str, y = mse, fill = cat, colour = cat)) + 
  geom_point() + stat_smooth(data = all) + facet_grid(cat ~ type) + 
  theme_bw() + theme(legend.position="none")


ggplot(subset(all, cat == 'pseudo-rand' & type == 'amta'), 
       aes(x = sos - str, y = mse, colour = type)) + 
  stat_smooth() + facet_wrap( ~ true_rank, nrow = 4) + 
  theme_bw() + geom_vline(xintercept = 0)


#### Strength of Team within Trial #####
all.trial <- ddply(all, .(trial, cat, type), summarise,
                   r1opp.str  = mean(sqrt((r1opp.str - str)^2)),
                   r2opp.str  = mean(sqrt((r2opp.str - str)^2)),
                   r3opp.str  = mean(sqrt((r3opp.str - str)^2)), 
                   r4opp.str  = mean(sqrt((r4opp.str - str)^2)),
                   allopp.str = mean(sqrt((sos - str)^2)),
                   sos.d      = mean(abs(70 - sos)),
                   mse        = mean(mse))

# with(subset(all, trial == 1), mean(sqrt((r1opp.str - str)^2)))

ggplot(all.trial, aes(x = sos.d, y = mse, fill = cat, colour = cat)) + 
  geom_point(alpha = 0.3) + facet_grid(cat ~ type) + stat_smooth(colour = 'black') + 
  theme_bw() + theme(legend.position="none")

ggplot(all.trial, aes(x = sos.d, fill = cat, colour = cat)) + 
  geom_histogram() + facet_grid(cat ~ type) +
  theme_bw() + theme(legend.position="none")

