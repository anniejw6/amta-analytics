# rm(list=ls())
setwd("~/Dropbox/Tabulation Sub-Committee/Simulation")
source('sim_func.R')

# Simulate two tournaments with the same set of teams and Round 1 assignments
# Then, pair rounds 2-4 using AMTA and WPB

# Set base values
num.teams = 24
qualwin = 14 # quality-win threshold
base = 1 # Minimum number of ballots that teams start out with for wpb
num.trials = 500 # Number of simulated tournaments
sdev = 10 # SD of team strength
# sdev = seq(1, 51, by = 5)

amta.tot <- NULL
wpb.tot <- NULL

for (trial in 1:num.trials){
  for (k in 1:length(sdev)){
    amta <- NULL
    wpb <- NULL
    
    #Generate data frames
    amta <- data.frame(team = 1:num.teams)
    amta[,genNames("AMTA")] <- NA
    
    wpb <- data.frame(team = 1:num.teams, base = base)
    wpb[,genNames("wpb")] <- NA
    
    # Generate Teams and Strength
    amta$str <- wpb$str <- round(rnorm(num.teams, 70, sdev[k]), 0)
    amta$true_rank <- wpb$true_rank <- rank(-amta$str)
    
    # Pair Round 1
    amta[, c("r1side", "r1opp")] <- wpb[, c("r1side", "r1opp")] <- genR1(amta)
    
    for(i in 1:4){
      # Calculate PD
      amta[, paste0('r',i, c("opp.str", "pd"))] <- calcPD(amta, i)
      wpb[, paste0('r',i, c("opp.str", "pd"))] <- calcPD(wpb, i)
      
      # Calculate WPB Measures
      wpb[, paste("r", i, c("round.pb", "cum.pb"), sep ="")] <- calcPB(i)
      wpb[, c(paste0("r", 1:4, "round.wpb"), paste0("r", i, "cum.wpb"))] <- calcWPB(i)
      
      # Calculate AMTA Measures 
      amta[, paste0("r", i, c("round.bal", "cum.bal"))] <- calcBal(i)
      amta[, c(paste0("r", 1:4, "round.cs"), paste0("r", i, "cum.cs"))] <- calcCS(i)
      
      # Calculate Point Differential
      amta[, paste0("r", i, "cum.pd")] <- calcCumPD(amta)
      wpb[, paste0("r", i, "cum.pd")] <- calcCumPD(wpb)
      
      # Rank Round 1
      amta[,paste0("r", i,"rank")] <- with(amta, rankTrad(get(paste0("r", i, "cum.bal")), 
                                                          get(paste0("r", i, "cum.cs")),
                                                          get(paste0("r", i, "cum.pd"))))
      wpb[,paste0("r", i,"rank")] <- with(wpb, rankWPB(get(paste0("r", i, "cum.wpb")), 
                                                       get(paste0("r", i, "cum.pb")),
                                                       get(paste0("r", i, "cum.pd"))))
      
      #Pair next round
      if(i < 4){
        amta[,paste0("r", i + 1, c("side","opp"))] <- pairTeams(amta, i + 1)
        wpb[,paste0("r", i + 1, c("side","opp"))] <- pairTeams(wpb, i + 1)    
      }
      
    }
    amta$trial <- trial
    wpb$trial <- trial
    
    amta$sdev <- sdev[k]
    wpb$sdev <- sdev[k]
    
    amta.tot <- rbind(amta.tot, amta)
    wpb.tot <- rbind(wpb.tot, wpb)
    
    print(trial)
  }
}