library(plyr)
library(reshape)

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

createData <- function(typ, s, base = NULL){
  
  if(typ == 'AMTA'){
    res <- data.frame(team = 1:num.teams)
  } else if(typ == 'wpb'){
    res <- data.frame(team = 1:num.teams, base = base)
  }
  
  res[ ,genNames(typ)] <- NA
  
  res$str <- s
  res$true_rank <- rank(s)
  
  return(res)
}

##### Round 1: Random Pairing ######

pairR1Wrap <- function(dat, typ){
  r1 <- c("r1side", "r1opp")
  
  if (typ == 'power'){
    dat[, r1]  <- r1power(amta)
  } else if (typ == 'fold'){
    dat[, r1] <- r1fold(amta)
  } else if (typ == 'envelope'){
    dat[, r1] <- r1envelope(amta)
  } else {
    dat[, r1] <- genR1(amta, num.teams)
  }
  
  dat$r1side <- ifelse(dat$r1side == 1, 'P', 'D')
  
  return(dat)
  
}

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
roundOutcomesWrap <- function(dat, type, r = i, qw = qualwin){
  
  # Calculate PD
  dat[, paste0('r',i, c("opp.str", "pd"))] <- calcPD(dat, r)
  
  if(type == 'amta'){
    
    # Calculate AMTA measures
    dat[, paste0("r", r, c("round.bal", "cum.bal"))] <- calcBal(r, dat)
    dat[, c(paste0("r", 1:4, "round.cs"), paste0("r", r, "cum.cs"))] <- calcCS(r, dat)
    
  }else if(type == 'wpb'){
    
    # Calculate WPB Measures
    dat[, paste("r", r, c("round.pb", "cum.pb"), sep ="")] <- calcPB(r, dat, qw)
    dat[, c(paste0("r", 1:4, "round.wpb"), paste0("r", r, "cum.wpb"))] <- calcWPB(r, dat)
    
  }
  
  # Calculate Cumulate PD
  dat[, paste0("r", r, "cum.pd")] <- calcCumPD(dat)
  
  # Rank Teams
  
  if(type == 'amta'){
    
    tmp <- with(dat, rankTrad(get(paste0("r", r, "cum.bal")), 
                              get(paste0("r", r, "cum.cs")),
                              get(paste0("r", r, "cum.pd"))))
  } else if(type == 'wpb'){
    
    tmp <- with(dat, rankWPB(get(paste0("r", r, "cum.wpb")), 
                             get(paste0("r", r, "cum.pb")),
                             get(paste0("r", r, "cum.pd"))))
    
  }
  
  dat[,paste0("r", r,"rank")] <- tmp
  
  
  return(dat)
}

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