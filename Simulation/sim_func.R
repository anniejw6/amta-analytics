library(plyr)
library(reshape)

##### Wrapper #########
sim <- function(num.trials,
                type = c('random', 'power', 'fold', 'envelope', 'pseudo-rand'),
                str = c(81:71, 70, 70, 69:59),
                qualwin = 14,
                sdev = 10,
                teams = NULL,
                pp = F){
  
  type <- match.arg(type)
  
  # Set base values
  num.teams = 24
  base = 1 # Minimum number of ballots that teams start out with for wpb
  
  # Set Team Values and Impermissibles
  if(is.null(teams)){
    teams <- data.frame(
      University = 1:num.teams,
      Team = 1:num.teams,
      TeamName = 1:num.teams
    )
  }
  
  amta.tot <- NULL
  wpb.tot <- NULL
  meta.tot <- NULL
  
  if(type == 'random'){
    num.trials <- num.trials/length(sdev)
  } 
  
  for (trial in 1:num.trials){
    print(paste("TRIAL", trial))
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
        
        amta$str <- wpb$str <- sample(str)
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
      
      # Fix Sides
      amta$r1side <- ifelse(amta$r1side == 1, 'P', 'D')
      wpb$r1side <- ifelse(wpb$r1side == 1, 'P', 'D')
      
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
            if(nrow(amta_prim_tab) %% 2 == 1){
              
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
  }
  
  if(type != 'random'){
    amta.tot$sdev <- sd(str)
    wpb.tot$sdev <- sd(str)
  }
  
  amta.tot$cat <- type
  wpb.tot$cat <- type
  
  return(list(amta = amta.tot, wpb = wpb.tot))
}


#### Set-up ######
# Generate Column Names
genNames = function(type = "AMTA"){
  if(type == "AMTA") opts <- c("round.bal", "cum.bal", "cum.cs")
  else opts <- c("round.wpb", "cum.wpb","round.pb","cum.pb")
  x <- NULL
  for(i in 1:4){
    x <- c(x, paste("r",i,c("side","opp", "opp.str", "pd", opts, "cum.pd", "rank"), sep = ""))
  }
  return(x)
}

##### Round 1: Random Pairing ######
# For Round 1, assign teams randomly
genR1 = function(mat1, num.teams){
  mat1$r1side <- sample(rep(0:1, num.teams/2), num.teams)
  mat1$r1opp[mat1$r1side == 0] <- 
    sample(mat1$team[mat1$r1side == 1], num.teams/2)
  mat1$r1opp[mat1$r1side == 1] <- 
    match(mat1$team[mat1$r1side == 1], mat1$r1opp)
  return(mat1[,c("r1side", "r1opp")])
}

r1fold <- function(mat1){
  
  # Order
  mat1 <- mat1[order(mat1$true_rank), ]
  
  # Separate into brackets
  mat1$ranks <- 1:nrow(mat1)
  
  #Match Pairs
  mat1$r1opp[1:12] <- sample(mat1$team[13:24], 12)
  
  # Assign Sides
  mat1$r1side[1:12] <- as.vector(replicate(6, sample(0:1,2)))
  
  # Match Assignments
  m <- match(mat1$team[is.na(mat1$r1opp)], mat1$r1opp)
  mat1$r1opp[is.na(mat1$r1opp)] <- mat1$team[m]
  mat1$r1side[is.na(mat1$r1side)] <- ifelse(mat1$r1side[m] == 0, 1, 0)
  
  return(mat1[order(mat1$team), c("r1side", "r1opp")])
  
}

r1power <- function(mat1){
  
  # Order
  mat1 <- mat1[order(mat1$true_rank), ]
  
  mat1$ranks <- 1:nrow(mat1)
  
  mat1$r1opp <- c(matrix(c(mat1$team[c(FALSE, TRUE)], 
                           mat1$team[c(TRUE, FALSE)]), 
                         2, byrow = T))
  
  mat1$r1side<- as.vector(replicate(12, sample(0:1,2)))
  
  mat1 <- mat1[order(mat1$team),]
  
  return(mat1[order(mat1$team),c("r1side", "r1opp")])
}

r1envelope <- function(mat1){
  
  #Order
  mat1 <- mat1[order(mat1$true_rank), ]
  mat1$ranks <- 1:nrow(mat1)
  
  #Match Pairs
  mat1$r1opp <- mat1$team[nrow(mat1):1]
  
  # Assign Sides
  mat1$r1side[1:12] <- as.vector(replicate(6, sample(0:1,2)))
  
  # Match Assignments
  m <- match(mat1$team[is.na(mat1$r1side)], mat1$r1opp)
  mat1$r1side[is.na(mat1$r1side)] <- ifelse(mat1$r1side[m] == 0, 1, 0)
  
  return(mat1[order(mat1$team),c("r1side", "r1opp")])
  
}

##### Calculating Strength ######

# Calculate Expected Point Differential
calcPD = function(mat1, round){
  
  oppstr <- with(mat1, str[match(get(paste0("r", round, "opp")), team)])
  
  capPD <- function(x) {
    if(x > 140) x <- 140
    else if(x < -140) x <- -140
    return(x)
  }
  
  pd <- with(mat1, sapply(str - oppstr, function(x) capPD(x)))
  
  return(data.frame(oppstr, pd))
}

#### WPB Measures #####
#Calculate PB and WPB
calcPB <- function(round, mat1 = wpb, max = qualwin){
  
  genPB <- function(x, max = max){
    if(x > max) pb <- 1
    else if(x < max * -1) pb <- 0
    else if(x >= 0) pb <- 0.5*(1 + logb(1+x, max + 1))
    else pb <- 0.5*(1 - logb(1 - x, max + 1))
    return(pb)
  }
  
  mat1[,paste0("r",round,"round.pb")] <- 
    with(mat1, sapply(get(paste0("r", round, "pd")), function(x) genPB(x, max)))
  pbtot = apply(mat1[, c("base", paste0("r", 1:4, "round.pb"))], 1, function(x) sum(x, na.rm= TRUE))
  return(data.frame(mat1[,paste("r",round,"round.pb", sep ="")], pbtot))
}


calcWPB <- function(round, mat1 = wpb){
  
  # Multiply each round's PB by the cumulative PB of the team you faced in that round
  # EX, After Round 3, you multiply Round 1 PB earned * Cumulative PB of Round 1 Opponent after Round 3,
  # including the adjustment
  
  wpb.round = matrix(NA, nrow = nrow(mat1), ncol = 4)
  if(round > 1){
    for (j in 1:4){
      wpb.round[, j] <- with(mat1, get(paste("r", j, "round.pb", sep = ""))) * 
        with(mat1, get(paste0("r", round, "cum.pb"))[match(get(paste0("r", j, "opp")),team)])
    }
  }
  wpb.cum = apply(wpb.round, 1, function(x) sum(x, na.rm= TRUE))
  return(data.frame(wpb.round, wpb.cum))
}

##### AMTA Measures ######
calcBal <- function(round, mat1 = amta){
  genBal <- function(x){
    if (x == 0) x <- 0.5
    else if(x > 0) x <- 1
    else x <- 0
    return(x)
  }
  mat1[,paste0("r",round,"round.bal")] <- 
    with(mat1, sapply(get(paste0("r", round, "pd")), function(x) genBal(x)))
  bal.tot = apply(mat1[, paste0("r", 1:4, "round.bal")], 1, function(x) sum(x, na.rm= TRUE))
  return(data.frame(mat1[,paste0("r",round,"round.bal")], bal.tot))
}

calcCS <- function(round, mat1 = amta){
  cs.round = matrix(NA, nrow = nrow(mat1), ncol = 4)
  if(round > 1){
    for (i in 1:4){
      cs.round[, i] <- with(mat1, get(paste0("r", round, "cum.bal"))[match(get(paste0("r", i, "opp")), team)])
    }
  }
  cs.cum = apply(cs.round, 1, function(x) sum(x, na.rm= TRUE))
  return(data.frame(cs.round, cs.cum))
}

calcCumPD <- function(mat1){
  apply(mat1[,paste("r", 1:4, "pd", sep = "")], 1, function(x) sum(x, na.rm = TRUE))
}

#### Ranking Teams #####
# WPB: Use PB and WPB.
rankWPB <- function(wpb, pb, pd){
  wpb[is.na(wpb)] <- 0
  pb[is.na(pb)] <- 0
  pd[is.na(pd)] <-0
  rank(-1*as.numeric(interaction(wpb, pb, pd, drop = TRUE, lex.order=TRUE)))
}
# Traditional: Ballots won, then CS, then PD
rankTrad <- function(bal, cs, pd){
  bal[is.na(bal)] <- 0
  cs[is.na(cs)] <- 0
  pd[is.na(pd)] <-0
  rank(-1*as.numeric(interaction(bal, cs, pd, drop = TRUE, lex.order=TRUE)))
}

#### Power-Match Teams #####

# Pairing
addPairs <- function(finalPair){
  
  finalPair <- finalPair[order(finalPair$trial), ]
  d.teams <- subset(finalPair, side == 'D')
  p.teams <- subset(finalPair, side == 'P')
  
  d.teams$opp <- p.teams$team
  p.teams$opp <- d.teams$team
  
  x <- c('team', 'side', 'opp')
  x <- rbind(d.teams[, x], p.teams[, x])
  x <- x[order(x$team), ]
  
  return(x[ , c('side', 'opp')])
  
}