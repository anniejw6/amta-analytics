#' ---
#' title: "Round Pairing"
#' author: "ajw"
#' ---
#' 
#' 

#' This is the *next* round
round <- 4 

source('pair_func.R')
teams <- 'https://docs.google.com/spreadsheets/d/1REb82IzLPC3S7n93CntfAaqBETdSCDXoO6DKS9VV0ro/pubhtml?gid=0&single=true'
tab   <- 'https://docs.google.com/spreadsheets/d/1REb82IzLPC3S7n93CntfAaqBETdSCDXoO6DKS9VV0ro/pubhtml?gid=2080568749&single=true'

teams <- readSpreadsheet(teams)
df <- cleanTab(readSpreadsheet(tab))

tab <- data.frame(
  team = df$TEAM,
  side = df[[paste0('R', round - 1, '_SIDE')]],
  wpb  = as.numeric(df[[paste0('R', round - 1, '_RT_WPB')]]),
  pb   = as.numeric(df[[paste0('R', round - 1, '_RT_PB')]]),
  pd   = as.numeric(df[[paste0('R', round - 1, '_RT_PD')]]),
  stringsAsFactors = F
)

tab$rank <- rankWPB(wpb = 'wpb',
                    pb  = 'pb',
                    pd  = 'pd', dat = tab, r = round)

# Create impermissibles
impermiss <- rbind(sameSchool(teams),
                   pastOpp(df))

pair <- data.frame(P_team = rep(NA, nrow(df)/2),
                   D_team = NA)

#if round is side-constrained:
if(round %in% c(2,4)){
  
  #rank sides separately
  needP <- tab[tab$side == 'D', ]
  needD <- tab[tab$side == 'P', ]
  
  #pair highest versus highest
  pair <- teamMeta(pair, needP[order(needP$rank), ], side = 'P', round = round)
  pair <- teamMeta(pair, needD[order(needD$rank), ], side = 'D', round = round)
  
} else {
  
  #rank teams together
  tab <- tab[order(tab$rank), ]
  
  #pair 1 vs 2, 3 vs 4, etc
  pair <- teamMeta(pair, tab[c(T, F), ], side = 'P', round = round)
  pair <- teamMeta(pair, tab[c(F, T), ], side = 'D', round = round)
  
}

# Find impermissibles
pair$impermiss <- findImpermiss(pair, impermiss)

swaps <- data.frame(Team1 = NA, Team2 = NA, final = NA)

if(sum(pair$impermiss) == 0) print('No impermissibles')


# resolve impermissibles
while (sum(pair$impermiss) > 0){
  
  kable(pair)
  
  # Order trials by rank
  pair <- pair[order(pair$P_rank + pair$D_rank), ]
  
  # set trial_x = highest trial with impermissible
  trial_x <- pair[min(which(pair$impermiss == T)), ]
  
  # Compare swap distances based on WPB, PB, and PD
  possSwaps <- compareDist(all = tab, x = trial_x, round = round)  
  
  writeLines('/n')
  print(head(possSwaps))
  
  repeat{

    # Set proposed_swap = minimum distance swap
    proposedSwap <- possSwaps[1, ]
    
    # If it's allowed
    if(!paste0(proposedSwap$p, proposedSwap$d) %in% swaps$final){
      break
    }
    
    # If it's not allowed, remove proposed_swap from possible
    possSwaps <- possSwaps[-1, ]
  }
  
  #  make proposed_swap
  pair <- makeSwap(newSwap = proposedSwap, old = trial_x, dat = pair)
  
  #  insert proposed_swap in SWAP
  swaps <- insertSwap(neW = proposedSwap, dat = swaps)

  #  set n = number of impermissibles
  pair$impermiss <- findImpermiss(pair, impermiss)
}