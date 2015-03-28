##### Wrapper #########
sim <- function(num.trials,
  type = c('random', 'power', 'fold', 'envelope'),
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
  wpb.tot <- NULL
  
  if(wpb_opt == T){
    # Base Ballot Value for WPB
    base = 1 
  }
  
  for (trial in 1:num.trials){

    print(paste("TRIAL", trial))
    
    #Generate data frames
    amta <- createData('AMTA', str, b = NULL, nt = num.teams)
    
    # Pair Round 1 
    amta <- pairR1Wrap(amta, type)
    
    if(wpb_opt == T){

      #Generate data frames
      wpb <- createData('wpb', str, b = base, nt = num.teams)
      
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
          wpbtab <- tabProcessWrap(wpb, trad = F, rr = nextRound, cf = coinTie)
          
          # Define Impermissibles
          wpbImpermiss <- rbind(sameSchool(teams), pastOpp(wpb, nextRound))
        }
        
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

          # # Deal with Odd Numbes
          tmp <- fixOdd(amta_prim_tab, amta_secd_tab, b = b)

          amtaFinalPair <- wrapPowerPair(tmp$prim, tmp$secd, amtaImpermiss, 
                  nextRound, coinR3)

        } else {

          # Initial Pairing
          amtaPair <- initialPair(amtatab, nextRound, coinR3)
          
          # Resolve Impermissibles
          amtaFinalPair <- impermissWrap(amtaPair, amtaImpermiss, round = nextRound)
          
        }

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
    meta.tot <- rbind(meta.tot, meta)
    
    
    if(wpb_opt == T){
      wpb$trial <- trial
      }

    }

    return(list(amta = amta.tot, wpb = wpb.tot, meta = meta.tot, type = type))

  }
