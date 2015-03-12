cleanTab <- function(dat, amta){
  
  # Clean up columns
  del <- c(paste0('S.', 1:3), '','S', 'CURRENT_WPB', 'CURRENT_RANK', 
           'CURRENT_BAL', 'CURRENT_CS', 'CURRENT_PD')
  df <- dat[, !colnames(dat) %in% del]
  df <- subset(df, TEAM != '')
  
  # Deal with NAs
  df[df == '#N/A'] <- NA
  df[df == 'NA'] <- NA
  
  # Get rid of factors
  i <- sapply(df, is.factor)
  df[i] <- lapply(df[i], as.character)
  
  return(df)
    
  }

tabSumm <- function(df, amta, round){
  
  tab <- data.frame(
    team = df$team,
    side = df[[paste0('r', round - 1, 'side')]]
    )
  if(amta == F){
    
    x <- data.frame(
      crit1  = as.numeric(df[[paste0('r', round - 1, 'cum.wpb')]]),
      crit2   = as.numeric(df[[paste0('r', round - 1, 'cum.pb')]]),
      crit3   = as.numeric(df[[paste0('r', round - 1, 'cum.pd')]]),
      stringsAsFactors = F
    )
    
  } else {
    x <- data.frame(
      crit1  = as.numeric(df[[paste0('r', round - 1, 'cum.bal')]]),
      crit2   = as.numeric(df[[paste0('r', round - 1, 'cum.cs')]]),
      crit3   = as.numeric(df[[paste0('r', round - 1, 'cum.pd')]]),
      stringsAsFactors = F
    )
  }
  return(cbind(tab, x))
}

# Create data-frame of impermissible based on same school
sameSchool <- function(df){
  teams <- subset(df, University %in% University[duplicated(University)])
  x <- ddply(teams, .(University), function(x){
    res <- t(combn(x[['TeamName']], 2))
    oth <- res[, c(2, 1)]
    res <- data.frame(rbind(res, oth))
    return(res)
  })
  x$University <- NULL
  colnames(x) <- c('Team1', 'Team2')
  return(x)
}

# Create data-frame of impermissible based on past opponents
pastOpp <- function(df, round){
  
  # Opponents up to that round
  impermiss <- df[, c('team', paste0('r',1:(round - 1),'opp'))]
  impermiss <- melt(impermiss, id = 'team')
  
  # Get rid of random variable
  impermiss$variable <- NULL
  
  # Subset to rounds that have been paired
  impermiss <- subset(impermiss, !is.na(value))
  
  # Clean-up
  colnames(impermiss) <- c('Team1', 'Team2')
  
  return(impermiss)
}

initialPair <- function(tab, round, coin){
  pair <- tab
  
  #if round is side-constrained:
  if(round %in% c(2,4)){
    
    #rank sides separately
    pair$side <- ifelse(tab$side == 'P', 'D', 'P')  
    pair <- ddply(pair, .(side), mutate, rank = rank(rank))
    
    #pair highest versus highest
    pair <- pair[order(pair$rank, pair$side), ]
    
  } else {
    
    #rank teams together
    pair <- pair[order(pair$rank), ]
    
    #pair 1 vs 2, 3 vs 4, etc
    if(coin == 'heads'){
      pair$side <- rep(c('P', 'D'))
    } else {
      pair$side <- rep(c('D', 'P'))
    }
    
    
  }
  
  pair$trial <- rep(1:(nrow(pair)/2), each = 2)
  return(pair)
}

### Make Pairs look pretty ###
pairPretty <- function(dat = pair, amta = amta, impermiss = T){
  
  # Order by Trial
  dat <- dat[order(dat$trial, dat$side), ]
  
  # Set column names
  if(amta == T){
    crits <- c('Ballots', 'CS', 'PD')
  } else {
    crits <- c('WPB', 'PB', 'PD')
  }
  
  x <- colnames(dat)
  colnames(dat)[c(grep('crit1', x), 
                  grep('crit2', x), 
                  grep('crit3', x))] <- crits
  
  
  # Stack P and D
  p <- subset(dat, side == 'P')
  colnames(p) <- paste0('p_', colnames(p))
  d <- subset(dat, side == 'D')
  colnames(d) <- paste0('d_', colnames(d))
  x <- cbind(p, d)
  
  # Clean up Columns
  x <- x[, !grepl('side', colnames(x))]
  x <- x[, c('p_trial', 'p_team', 'd_team', 
             paste0('p_', crits), 'p_rank', 
             paste0('d_', crits), 'd_rank')]
  colnames(x)[1] <- 'trial'
  
  # Add Impermissibles
  if(impermiss == T){
    x$impermiss <- p$p_impermiss
  }
  
  # Round 
  i <- which(colnames(x) %in% c(paste0('p_', crits), paste0('d_', crits)))
  x[i] <- lapply(x[i], function(x) round(x, 3))
  
  return(x)
}

# rank teams by WPB, PB, PD, coin-flip
rankMT <- function(dat, 
                   crit1 = 'crit1', crit2 = 'crit2', crit3 = 'crit3', 
                   r = round, coinflip = 'heads'){
  
  if(coinflip == 'heads'){
    coinflip <- rank(dat$team)
  #  print('In case of ties, larger team number gets higher rank.')
  }else {
    coinflip <- -rank(dat$team)
   # print('In case of ties, lower team number gets higher rank.')
  }
  
  dat[is.na(dat)] <- 0
  crit1 <- dat[[crit1]]
  crit2 <- dat[[crit2]]
  crit3 <- dat[[crit3]]
  
  return(rank(-1*as.numeric(
    interaction(crit1, crit2, crit3, coinflip,
                drop = TRUE, lex.order = TRUE)))) 
}

# Calculate distance by WPB, PB, PD
# Returns 
compareDist <- function(x = trial_x, all = pair, round = round, amta = F){

  # Minor Cleaning
  all[is.na(all)] <- 0
  x[is.na(x)] <- 0

  # Create all possible matches 
  calcDist <- function(x, all, sideT, roundNum = round){
    
    x <- subset(x, side == sideT)
    
    if(roundNum %% 2 == 0){
      all <- subset(all, side == sideT)
    }
    
    if(sideT == 'D'){
      cat <-  'Keep P, Swap D'
    } else {
      cat <- 'Keep D, Swap P'
    }
    return(
      data.frame(
        dist_crit1 = abs(all$crit1 - x$crit1),
        dist_crit2 = abs(all$crit2 - x$crit2),
        dist_crit3 = abs(all$crit3 - x$crit3),
        dist_rank = abs(all$newRank - x$newRank),
        cat = cat
        )
    )
  }
  
  if(round %% 2 == 0){ # If round is side-constrained
    
    # Compare against teams of the same side
    dSwap <- data.frame(p = x$team[x$side == 'P'],
                        d = all$team[all$side == 'D'])
    dSwap <- cbind(dSwap, calcDist(x = x, all = all, sideT = 'D'))
    
    pSwap <- data.frame(p = all$team[all$side == 'P'],
                        d = x$team[x$side == 'D'])
    pSwap <- cbind(pSwap, calcDist(x = x, all = all, sideT = 'P'))
    
  } else {
    # Compare against all teams
    dSwap <- data.frame(p = x$team[x$side == 'D'],
                        d = all$team)
    dSwap <- cbind(dSwap, calcDist(x = x, all = all, sideT = 'D'))
    
    pSwap <- data.frame(p = all$team,
                        d = x$team[x$side == 'D'])
    pSwap <- cbind(pSwap, calcDist(x = x, all = all, sideT = 'P'))
    
  }
  res <- unique(rbind(dSwap, pSwap))
  
  # Get rid of factors
  i <- sapply(res, is.factor)
  res[i] <- lapply(res[i], as.character)
  
  # Get rid of matches where you're swapping the original ones,
  # or where you're matching against the same team.
  res <- subset(res, p != d)
  res <- subset(res, !paste0(res$p, res$d) %in% c(paste0(x$team, collapse =''), 
                                          paste0(rev(x$team), collapse = '')))
  
  # Sort by distance
  res <- res[order(res$dist_rank, res$dist_crit1, res$dist_crit2, res$dist_crit3), ]
  res <- subset(res, dist_rank > 0) # This is the no-swap option

  return(res)
}




####### Wrapper for Finding Impermissibles #####

impermissWrap <- function(pair, impermiss, round){
  
  # Find impermissibles
  pair$impermiss <- findImpermiss(pair, impermiss)
  
  # Set value to store swaps
  swaps <- data.frame(Team1 = NA, Team2 = NA, final = NA)
  
  # If there are no impermissibles,
  if(sum(pair$impermiss) == 0){
    
    #writeLines('No impermissibles!')
    
  } else {
    
    pair$newRank <- pair$rank
    
    # resolve impermissibles
    while (sum(pair$impermiss) > 0){
      
      # Print pairings at start
      #writeLines('Current List of Pairings')
      #print(pairPretty(pair, amta = amta))
      
      # set trial_x = highest trial with impermissible
      trial_x <- pair[pair$trial == pair$trial[min(which(pair$impermiss == T))], ]
      
      # Compare swap distances based on WPB, PB, and PD
      possSwaps <- compareDist(x = trial_x, all = pair, round = round, amta = F)  
      
      repeat{
        
        # Set proposed_swap = minimum distance swap
        proposedSwap <- possSwaps[1, ]
        
        # proposed Swap
        #writeLines('Proposed Swap')
        #print(proposedSwap)
        
        # If it's allowed
        test <- gsub(' ', '',
                     paste(swapList(newSwap = proposedSwap, oldSwap = trial_x), 
                           collapse = '-'))
        if(!test %in% swaps$final){
          break # Move on
        }
        
        # If it's not allowed, remove proposed_swap from possible
        #writeLines('Proposed swap is not possible!\n')
        possSwaps <- possSwaps[-1, ]
      }
      
      #  make proposed_swap
      pair <- makeSwap(newSwap = proposedSwap, old = trial_x, dat = pair)
      
      #  insert proposed_swap in SWAP
      swaps <- insertSwap(newSwap = proposedSwap, oldSwap = trial_x, dat = swaps)
      
      #  set n = number of impermissibles
      pair$impermiss <- findImpermiss(pair, impermiss)
    }
  }
  
  return(pair)
  
}


# Find Impermissibles
findImpermiss <- function(dat = pair, impermissibles = impermiss){
  
  notOkay <- paste0(impermissibles[, 1], impermissibles[, 2])
  
  res <- rep(F, nrow(dat))
  
  for(i in 1:nrow(dat)){
    
    proposedPair <- paste(dat$team[dat$trial == dat$trial[i]], collapse = '')
    
    if(proposedPair %in% notOkay){
      res[i] <- T
    }
    
  }
  return(res)
}

# Make proposed swap
makeSwap <- function(newSwap = proposedSwap, old = trial_x, dat = pair){
  
  swap <- function(swapSide = 'P', dat = dat){
    
    # Get row numbers
    oldD <- which(dat$team == old$team[old$side == swapSide])
    newD <- which(dat$team == newSwap[[tolower(swapSide)]])
    
    #writeLines('\nOriginal values\n')
    #printTab(xtable(dat[c(oldD, newD), ]))
    
    # Make the swap
    dat$trial[c(oldD, newD)] <- dat$trial[rev(c(oldD, newD))]
    dat$side[c(oldD, newD)] <-  dat$side[rev(c(oldD, newD))]
    dat$newRank[c(oldD, newD)] <-  dat$newRank[rev(c(oldD, newD))]
    
    #writeLines('\nNew values\n')
    #printTab(xtable(dat[c(oldD, newD), ]))
    
    return(dat)
  }
  
  if(newSwap$cat == 'Keep D, Swap P'){
    #writeLines(paste('\nSwapping Pros team', old$P_team, 'for', newSwap$p))
    side = 'P'
  } else {
    #writeLines(paste('\nSwapping Def team', old$D_team, 'for', newSwap$d))
    side = 'D'
  }
  
  return(swap(swapSide = side, dat = dat))
  
}

# Generate Swap
swapList <- function(newSwap, oldSwap){
  
  if(newSwap$cat == 'Keep P, Swap D'){
    neW <- c(team1 = newSwap$d,
             team2 = oldSwap$team[oldSwap$side == 'D'])
  } else {
    neW <- c(team1 = newSwap$p,
             team2 = oldSwap$team[oldSwap$side == 'P'])
  }
  return(neW)
}

# Insert Swaps
insertSwap <- function(newSwap = proposedSwap, oldSwap = trial_x, dat = swaps){
  
  x <- swapList(newSwap = newSwap, oldSwap = oldSwap)
  
  # Create out combinations
  news <- data.frame(Team1 = x,
                     Team2 = rev(x),
                     final = NA,
                     stringsAsFactors = F)
  # Add to existing
  dat <- rbind(dat, news)
  # Get rid of NAs
  dat <- subset(dat, !is.na(Team1))
  # Create concatenation
  dat$final <- apply(dat, 1, function(x){
    gsub(' ', '', paste(x[1], x[2], sep = '-'))
  })
  return(dat)
}