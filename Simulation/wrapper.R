##### Wrapper #########
sim <- function(num.trials,
                type = c('random', 'power', 'fold', 'envelope', 'pseudo-rand'),
                str = c(81:71, 70, 70, 69:59),
                qualwin = 14,
                sdev = 10,
                teams = NULL,
                pp = F,
                wpb_opt = F){
  
  type <- match.arg(type)
  
  # Set base values
  num.teams = 24
  if(wpb_opt == T){
    base = 1 # Minimum number of ballots that teams start out with for wpb
  }
  
  # Set Team Values and Impermissibles
  if(is.null(teams)){
    teams <- data.frame(
      University = 1:num.teams,
      Team = 1:num.teams,
      TeamName = 1:num.teams
    )
  }
  
  # Store Data
  amta.tot <- NULL
  meta.tot <- NULL
  if(wpb_opt == T){
    wpb.tot <- NULL
  }
  
  
  if(type == 'random'){
    num.trials <- num.trials/length(sdev)
  }
  
  for (trial in 1:num.trials){
    
    print(paste("TRIAL", trial))
    
    #Generate data frames
    amta <- createData('AMTA')
    if(wpb_opt == T){
      wpb <- createData('wpb', base = base)
    }
    
    # Generate Teams and Strength 
    amta$str <- str
    amta$true_rank <- rank(str)
    
    if(wpb_opt == T){
      wpb$str <- str
      wpb$true_rank <- rank(str)
    }
    
    # Pair Round 1 
    # Round 1
    amta <- pairR1Wrap(amta, type)
    if(wpb_opt == T){
      wpb <- pairR1Wrap(wpb, type)
    }
    
    for(i in 1:4){
      #print(paste("ROUND", i))
      
      ##### Simulate Outcomes #####
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
      
      # Rank Teams (not for Pairing)
      amta[,paste0("r", i,"rank")] <- with(amta, rankTrad(get(paste0("r", i, "cum.bal")), 
                                                          get(paste0("r", i, "cum.cs")),
                                                          get(paste0("r", i, "cum.pd"))))
      wpb[,paste0("r", i,"rank")] <- with(wpb, rankWPB(get(paste0("r", i, "cum.wpb")), 
                                                       get(paste0("r", i, "cum.pb")),
                                                       get(paste0("r", i, "cum.pd"))))
      
      #Pair next round
      if(i < 4){
        
        ##### Pairing #######
        coinTie <- 'heads'
        coinR3  <- 'heads'
        
        meta <- data.frame(
          trial = trial,
          coinTie = 'heads',
          coinR3 = 'heads'
        )
        
        nextRound <- i + 1
        
        # Re-Format
        amtatab <- tabSumm(amta, amta = T, round = nextRound)
        wpbtab  <- tabSumm(wpb, amta = F, round = nextRound)
        
        # Rank Teams
        amtatab$rank <- rankMT(dat = amtatab, 
                               crit1 = 'crit1', crit2 = 'crit2', crit3 = 'crit3', 
                               r = nextRound, coinflip = coinTie)
        
        wpbtab$rank <- rankMT(dat = amtatab, 
                              crit1 = 'crit1', crit2 = 'crit2', crit3 = 'crit3', 
                              r = nextRound, coinflip = coinTie)
        
        # Define Impermissibles
        amtaImpermiss <- rbind(sameSchool(teams),
                               pastOpp(amta, nextRound))
        
        wpbImpermiss <- rbind(sameSchool(teams),
                              pastOpp(wpb, nextRound))
        
        if(i == 3 && pp == T){
          
          #### AMTA Power - Protecting #####
          
          b <- amtatab$crit1[order(amtatab$crit1, decreasing = T)]
          
          #  Find Thresholds
          first_out <- b[7] # Assuming ORCs
          
          # Separate into Brackets
          
          sec <- which(amtatab$crit1 >= (first_out + 1.5))
          sec <- c(sec, which(amtatab$crit1 <= (first_out - 1)))
          
          amta_secd_tab <- amtatab[sec, ]
          amta_prim_tab <- amtatab[-sec, ]
          
          # Deal with Odd Numbes
          if(nrow(amta_prim_tab) %% 2 == 1){ ### TODO Needs to compare whether we are side-balanced
            
            amta_prim_tab <- amta_prim_tab[order(amta_prim_tab$crit1, decreasing = T),]
            nr <- nrow(amta_prim_tab)
            
            if(abs(amta_prim_tab$crit1[nr] - b[6]) > 1){ # assuming ORCs
              
              amta_secd_tab <- amta_secd_tab[order(amta_secd_tab$crit1, decreasing = T),]
              
              amta_prim_tab <- rbind(amta_prim_tab,
                                     amta_secd_tab[1,])
              
              amta_secd_tab <- amta_secd_tab[-1, ]
              
            } else {
              
              amta_secd_tab <- rbind(amta_secd_tab,
                                     amta_prim_tab[nr, ])
              
              amta_prim_tab <- amta_prim_tab[-nr, ]
              
            }
            
          }
          
          # Inital Pairing
          amta_prim_pair <- initialPair(amta_prim_tab, nextRound, coinR3, pp = T)
          amta_secd_pair <- initialPair(amta_secd_tab, nextRound, coinR3, pp = F)
          
          # Resolve Impermissibles
          amtaprimFinal <- impermissWrap(amta_prim_pair, amtaImpermiss, round = nextRound)
          amtasecdFinal <- impermissWrap(amta_secd_pair, amtaImpermiss, round = nextRound)
          
          # Fix Trial Numbers
          amtasecdFinal$trial <- amtasecdFinal$trial + nrow(amtaprimFinal)/2
          
          # Recombine
          amtaFinal <- rbind(amtaprimFinal, amtasecdFinal)
          
          #### No Change to WPB ######
          
          # Initial Pairing
          wpbPair  <- initialPair(wpbtab, nextRound, coinR3)
          
          # Resolve Impermissibles
          wpbFinalPair  <- impermissWrap(wpbPair, wpbImpermiss, round = nextRound)
          
          
        } else {
          
          # Initial Pairing
          amtaPair <- initialPair(amtatab, nextRound, coinR3)
          wpbPair  <- initialPair(wpbtab, nextRound, coinR3)
          
          # Resolve Impermissibles
          amtaFinalPair <- impermissWrap(amtaPair, amtaImpermiss, round = nextRound)
          wpbFinalPair  <- impermissWrap(wpbPair, wpbImpermiss, round = nextRound)
          
          
        }
        
        # Add Final Pairs
        x <- paste0("r", i + 1, c("side","opp"))
        amta[, x] <- addPairs(amtaFinalPair)
        wpb[, x] <- addPairs(wpbFinalPair)
        
      }
      
    }
    
    amta$trial <- trial
    wpb$trial <- trial
    
    amta$sdev <- sdev[k]
    wpb$sdev <- sdev[k]
    
    amta.tot <- rbind(amta.tot, amta)
    wpb.tot <- rbind(wpb.tot, wpb)
    meta.tot <- rbind(meta.tot, meta)
    
    if(trial%%100 == 0) print(trial) 
    
  }
  
  if(type != 'random'){
    amta.tot$sdev <- sd(str)
    wpb.tot$sdev <- sd(str)
  }
  
  amta.tot$cat <- type
  wpb.tot$cat <- type
  
  return(list(amta = amta.tot, wpb = wpb.tot))
}
