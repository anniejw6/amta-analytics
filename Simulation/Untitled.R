sim <- function(num.trials,
                type = c('random', 'power', 'fold', 'envelope', 'pseudo-rand'),
                str = c(81:71, 70, 70, 69:59),
                qualwin = 14,
                sdev = 10){
  
  type <- match.arg(type)
  
  # Set base values
  num.teams = 24
  base = 1 # Minimum number of ballots that teams start out with for wpb
  
  amta.tot <- NULL
  wpb.tot <- NULL
  
  if(type == 'random'){
    num.trials <- num.trials/length(sdev)
  } 
  
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
      if(type == 'random'){
        
        amta$str <- wpb$str <- round(rnorm(num.teams, 70, sdev[k]), 0)
        amta$true_rank <- wpb$true_rank <- rank(-amta$str)
        
      } else {
        
        amta$str <- wpb$str <- str
        amta$true_rank <- wpb$true_rank <- rank(-amta$str)
      }
      
      # Pair Round 1 
      # Round 1
      r1 <- c("r1side", "r1opp")
      
      if(type %in% c('random', 'pseudo-rand')){
        amta[, r1] <- wpb[, r1] <- genR1(amta, num.teams)
      } else if (type == 'power'){
        amta[, r1] <- wpb[, r1] <- r1power(amta)
      } else if (type == 'fold'){
        amta[, r1] <- wpb[, r1] <- r1fold(amta)
      } else if (type == 'envelope'){
        amta[, r1] <- wpb[, r1] <- r1envelope(amta)
      }
      
      for(i in 1:4){
        # Calculate PD
        amta[, paste0('r',i, c("opp.str", "pd"))] <- calcPD(amta, i)
        wpb[, paste0('r',i, c("opp.str", "pd"))] <- calcPD(wpb, i)
        
        # Calculate WPB Measures
        wpb[, paste("r", i, c("round.pb", "cum.pb"), sep ="")] <- calcPB(i, wpb, qualwin)
        wpb[, c(paste0("r", 1:4, "round.wpb"), paste0("r", i, "cum.wpb"))] <- calcWPB(i, wpb)
        
        # Calculate AMTA Measures 
        amta[, paste0("r", i, c("round.bal", "cum.bal"))] <- calcBal(i, amta)
        amta[, c(paste0("r", 1:4, "round.cs"), paste0("r", i, "cum.cs"))] <- calcCS(i, amta)
        
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
      
      if(trial%%100 == 0) print(trial) 
    }
  }
  
  if(type != 'random'){
    amta.tot$sdev <- sd(str)
    wpb.tot$sdev <- sd(str)
  }
  
  amta.tot$cat <- type
  wpb.tot$cat <- type
  
  return(list(amta = amta.tot, wpb = wpb.tot))
}