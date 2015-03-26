##### Wrapper #########
sim <- function(num.trials,
                type = c('random', 'power', 'fold', 'envelope', 'pseudo-rand'),
                str = c(81:71, 70, 70, 69:59),
                qualwin = 14,
                teams = NULL,
                pp = F,
                wpb_opt = F){
  
  type <- match.arg(type)
  
  # Set base values
  num.teams = 24
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
    # Base Ballot Value for WPB
    base = 1 
  }
  
  for (trial in 1:num.trials){
    
    print(paste("TRIAL", trial))
    
    #Generate data frames
    amta <- createData('AMTA', str)
    
    # Pair Round 1 
    amta <- pairR1Wrap(amta, type)
    
    if(wpb_opt == T){
      
      #Generate data frames
      wpb <- createData('wpb', str, base = base)
      
      # Pair Round 1 
      wpb <- pairR1Wrap(wpb, type)
    }
    
    for(i in 1:4){
      
      ##### Simulate Outcomes #####
      amta <- roundOutcomesWrap(amta, type = 'amta', r = i, qw = NULL)
      
      if(wpb_opt == T){
        ##### Simulate Outcomes #####
        wpb  <- roundOutcomesWrap(wpb, type = 'wpb', r = i, qw = qualwin)
      }
      
      #Pair next round
      if(i < 4){
        
        ##### Pairing #######
        coinTie <- 'heads'
        coinR3  <- 'heads'
        
        meta <- data.frame(
          trial = trial,
          coinTie = coinTie,
          coinR3 = coinR3
        )
        
        nextRound <- i + 1
        
        # Reformat and Rank
        amtatab <- tabProcessWrap(amta, trad = T, rr = nextRound, cf = coinTie)
        
        # Define Impermissibles
        amtaImpermiss <- rbind(sameSchool(teams), pastOpp(amta, nextRound))
        
        if(wpb_opt == T){
          
          # Reformat and Rank
          wpbtab <- tabProcessWrap(wpb, trad = T, rr = nextRound, cf = coinTie)
          
          # Define Impermissibles
          wpbImpermiss <- rbind(sameSchool(teams), pastOpp(wpb, nextRound))
        }
        
        #         if(i == 3 && pp == T){
        #           
        #           #### AMTA Power - Protecting #####
        #           
        #           b <- amtatab$crit1[order(amtatab$crit1, decreasing = T)]
        #           
        #           #  Find Thresholds
        #           first_out <- b[7] # Assuming ORCs
        #           
        #           # Separate into Brackets
        #           
        #           sec <- which(amtatab$crit1 >= (first_out + 1.5))
        #           sec <- c(sec, which(amtatab$crit1 <= (first_out - 1)))
        #           
        #           amta_secd_tab <- amtatab[sec, ]
        #           amta_prim_tab <- amtatab[-sec, ]
        #           
        #           # Deal with Odd Numbes
        #           if(nrow(amta_prim_tab) %% 2 == 1){ ### TODO Needs to compare whether we are side-balanced
        #             
        #             amta_prim_tab <- amta_prim_tab[order(amta_prim_tab$crit1, decreasing = T),]
        #             nr <- nrow(amta_prim_tab)
        #             
        #             if(abs(amta_prim_tab$crit1[nr] - b[6]) > 1){ # assuming ORCs
        #               
        #               amta_secd_tab <- amta_secd_tab[order(amta_secd_tab$crit1, decreasing = T),]
        #               
        #               amta_prim_tab <- rbind(amta_prim_tab,
        #                                      amta_secd_tab[1,])
        #               
        #               amta_secd_tab <- amta_secd_tab[-1, ]
        #               
        #             } else {
        #               
        #               amta_secd_tab <- rbind(amta_secd_tab,
        #                                      amta_prim_tab[nr, ])
        #               
        #               amta_prim_tab <- amta_prim_tab[-nr, ]
        #               
        #             }
        #             
        #           }
        #           
        #           # Inital Pairing
        #           amta_prim_pair <- initialPair(amta_prim_tab, nextRound, coinR3, pp = T)
        #           amta_secd_pair <- initialPair(amta_secd_tab, nextRound, coinR3, pp = F)
        #           
        #           # Resolve Impermissibles
        #           amtaprimFinal <- impermissWrap(amta_prim_pair, amtaImpermiss, round = nextRound)
        #           amtasecdFinal <- impermissWrap(amta_secd_pair, amtaImpermiss, round = nextRound)
        #           
        #           # Fix Trial Numbers
        #           amtasecdFinal$trial <- amtasecdFinal$trial + nrow(amtaprimFinal)/2
        #           
        #           # Recombine
        #           amtaFinal <- rbind(amtaprimFinal, amtasecdFinal)
        #           
        #           #### No Change to WPB ######
        #           
        #           # Initial Pairing
        #           wpbPair  <- initialPair(wpbtab, nextRound, coinR3)
        #           
        #           # Resolve Impermissibles
        #           wpbFinalPair  <- impermissWrap(wpbPair, wpbImpermiss, round = nextRound)
        #           
        #           
        #         } else {
        
        # Initial Pairing
        amtaPair <- initialPair(amtatab, nextRound, coinR3)
        
        
        # Resolve Impermissibles
        amtaFinalPair <- impermissWrap(amtaPair, amtaImpermiss, round = nextRound)
        
        # Add Final Pairs
        x <- paste0("r", i + 1, c("side","opp"))
        amta[, x] <- addPairs(amtaFinalPair)
        
        if(wpb_opt == T){
          # Initial Pairing
          wpbPair  <- initialPair(wpbtab, nextRound, coinR3)
          # Resolve Impermissibles
          wpbFinalPair  <- impermissWrap(wpbPair, wpbImpermiss, round = nextRound)
          # Add Final Pairs
          wpb[, x] <- addPairs(wpbFinalPair)
        }
      }

    }
    
    #### STore Data #####
    if(trial%%100 == 0) print(trial) 
    
    amta$trial <- trial
    amta.tot <- rbind(amta.tot, amta)
    
    if(wpb_opt == T){
      wpb$trial <- trial
      wpb.tot <- rbind(wpb.tot, wpb)
    }
    
    meta.tot <- rbind(meta.tot, meta)
  
  }
  
 
  amta.tot$cat <- type
  wpb.tot$cat <- type
  
  return(list(amta = amta.tot, wpb = wpb.tot))
  
}
