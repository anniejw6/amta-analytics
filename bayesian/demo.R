library(rstan)
setwd("~/fun/amta-analytics/bayesian")

genMatch <- function(teams, strength, variance){
  
  team1 <- sample(teams, 1)
  team2 <- sample(teams[teams != team1], 1)
  
  str1 <- round(rnorm(1, strength[team1], variance[team1]))
  str2 <- round(rnorm(1, strength[team2], variance[team2]))
  
  return(c(team1 = team1, team2 = team2, 
           str1 = str1, str2 = str2,
           out = str1 - str2))
  
}

teams <- 1:10
strength <- round(rnorm(length(teams), 70, 20))
variance <- round(runif(length(teams), 10, 50))

df <- NULL

for(i in 1:24){
  df <- rbind(df, genMatch(teams, strength, variance))
}

df <- data.frame(df)

dat <- list(
  team = length(teams),
  match = nrow(df),
  team1 = df$team1,
  team2 = df$team2,
  #str1 = df$str1,
  #str2 = df$str2,
  out = df$out)

fit <- stan(file = 'model.stan', data = dat, 
            iter = 2000, chains = 4)

x <- extract(fit)

plot(strength, apply(x$str, 2, mean))