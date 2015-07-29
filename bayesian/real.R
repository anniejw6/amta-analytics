# Dependencies
library(reshape)
library(rstan)
library(ggplot2)
library(stringr)

# Data Import

data <- read.csv('/Users/civisemployee/fun/amta-analytics/data/1 - State College ORCS.csv')

# Reshape into Long
df <- NULL
for(i in 1:4){
  temp_data <- data[, c(paste0('r', i, 'side'),
                        'teamNumber',
                        paste0('r', i, 'opponent'), 
                        paste0('r', i, 'ballot1'),
                        paste0('r', i, 'ballot2'))]
  colnames(temp_data) <- c('side', 'team1', 'team2', 'out1', 'out2')
  temp_data$round <- i
  df <- rbind(df, temp_data)
}

df <- subset(df, side == 'P')

df1 <- melt(df, id = c('team1', 'team2', 'round', 'side'))

# Recode team numbers
teams <- data.frame(team = data$teamNumber, number = 1:nrow(data))
rec <- function(to_code, list_old, list_new){
  return(list_new[list_old == to_code])
}
df1$team1 <- sapply(df1$team1, function(x){
  rec(x, teams$team, teams$number)
})
df1$team2 <- sapply(df1$team2, function(x){
  rec(x, teams$team, teams$number)
})

# Run Stan
dat <- list(
  team = nrow(data),
  match = nrow(df1),
  team1 = df1$team1,
  team2 = df1$team2,
  #str1 = df$str1,
  #str2 = df$str2,
  out = df1$value)

fit <- stan(file = 'model.stan', data = dat, 
            iter = 2000, chains = 4)
print(fit)
x <- extract(fit)

# Plots
plot(with(data, totalWins + totalTies * 0.5), apply(x$str, 2, mean))

fit_str <- data.frame(x$str)
fit_var <- data.frame(x$vari)
colnames(fit_str) <- colnames(fit_var) <- paste0(as.character(data$teamName), '-', data$teamNumber)

sfit_str <- melt(fit_str[, sample(1:24, 5)])

ggplot(sfit_str, aes(x = value, fill = variable, group = variable)) + 
  stat_density(alpha = 0.4, position = 'dodge', adjust = 3)

# Top 6
rank <- data.frame(t(apply(fit_str, 1, function(x) rank(-1 * x))))
top6 <- data.frame(t(apply(rank, 1, function(x) x <= 6)))

x <- melt(apply(top6, 2, mean))
x$team <- row.names(x)
row.names(x) <- NULL
x <- x[order(x$value), ]
x$team <- factor(x$team, levels = x$team, ordered = T)
x$top6 <- c(rep(0, 18), rep(1, 6))
ggplot(x, aes(x = team, y = value, fill = factor(top6))) + 
  geom_bar(position = 'dodge', stat = 'identity') + theme_bw() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

# Abandoned
interval <- apply(fit_str, 2, function(x) quantile(x, c(0.25, 0.5, 0.75)))
sinterval <- melt(interval)
