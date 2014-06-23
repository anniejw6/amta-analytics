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
genR1 = function(mat1){
  mat1$r1side <- sample(rep(0:1, num.teams/2), num.teams)
  mat1$r1opp[mat1$r1side == 0] <- 
    sample(mat1$team[mat1$r1side == 1], num.teams/2)
  mat1$r1opp[mat1$r1side == 1] <- 
    match(mat1$team[mat1$r1side == 1], mat1$r1opp)
  return(mat1[,c("r1side", "r1opp")])
}

r1fold <- function(mat1){
  
  mat1$ranks <- 1:nrow(mat1)
  
  # mat1 <- mat1[order(mat1$ranks), ]

  mat1$r1opp <- c(sample(13:24, 12), sample(1:12, 12))
  
  mat1$r1side<- as.vector(replicate(12, sample(0:1,2)))
  
  mat1 <- mat1[order(mat1$team),]
  
  return(mat1[,c("r1side", "r1opp")])

}

r1power <- function(mat1){
  
  mat1$ranks <- 1:nrow(mat1)
  
 # mat1 <- mat1[order(mat1$ranks), ]
  
  mat1$r1opp <- c(matrix(c(mat1$team[c(FALSE, TRUE)], 
                           mat1$team[c(TRUE, FALSE)]), 
                         2, byrow = T))
 
 mat1$r1side<- as.vector(replicate(12, sample(0:1,2)))
 
 mat1 <- mat1[order(mat1$team),]
  
  return(mat1[,c("r1side", "r1opp")])
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
  mat1[,paste("r",round,"round.pb", sep ="")] <- 
    with(mat1, sapply(get(paste("r", round, "pd", sep = "")), function(x) genPB(x, max)))
  pbtot = apply(mat1[, c("base", paste("r", 1:4, "round.pb", sep = ""))], 1, function(x) sum(x, na.rm= TRUE))
  return(data.frame(mat1[,paste("r",round,"round.pb", sep ="")], pbtot))
}


calcWPB <- function(round, mat1 = wpb){
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
pairTeams = function(mat1, round){
  
  side = mat1[,paste("r",round - 1,"side", sep ="")]
  ranks = mat1[,paste("r",round - 1,"rank", sep ="")]
  team = mat1$team
  
  if (round %% 2 == 0){
    
    d.rank <- rank(ranks[side == 0], ties.method = "random")
    p.rank <- rank(ranks[side == 1], ties.method = "random")
    
    total <- rep(NA, length(side))
    
    total[side == 0] <- subset(team, side == 1)[match(d.rank, p.rank)]
    total[side == 1] <- match(team[side == 1], total)
    
    side.new <- (side - 1) * -1
    
    return(data.frame(side.new, total))
  } else {
    
    ranks <- rank(ranks, ties.method = "random")
    x <- data.frame(team, ranks)
    x <- x[order(x$ranks), ]
    x$side.new <- rep(0:1, length(x$team)/2)
    x$new <- c(matrix(c(x$team[x$side.new == 1], x$team[x$side.new == 0]), 2, byrow = T)) 
    x <- x[order(x$team),]
    
    return(data.frame(x$side.new, x$new))
  }
}