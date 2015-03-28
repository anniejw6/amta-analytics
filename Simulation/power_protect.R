###### FUNCTIONS ######
wrapPowerPair <- function(prim_tab, sec_tab, impermiss, 
        r = nextRound, c = coinR3){

        #### Pairing by Bracket #####

        # Inital Pairing
        prim_pair <- initialPair(prim_tab, r, c, pp = T)
        secd_pair <- initialPair(sec_tab, r, c, pp = F)

        # Resolve Impermissibles
        primFinal <- impermissWrap(prim_pair, impermiss, round = r)
        secdFinal <- impermissWrap(secd_pair, impermiss, round = r)

        # Fix Trial Numbers
        secdFinal$trial <- secdFinal$trial + nrow(primFinal)/2

        # Recombine
        final <- rbind.fill(primFinal, secdFinal)

        return(final)

}

fixOdd <- function(prim_tab, secd_tab, b){

        countSide <- function(dat){
                #### Count un-even
                return(c(num_p = sum(dat$side == 'P'),
                        num_d = sum(dat$side == 'D')))
        }

        #### Fix it when you have an odd number of teams in your primary bracket

        ss <- countSide(prim_tab)

        while(ss['num_d'] != ss['num_p']){ 

                xs <- ifelse(ss['num_d'] < ss['num_p'], 'P', 'D')
                rk <- with(prim_tab, max(rank[side == xs]))

                low <- prim_tab[prim_tab$rank == rk, ]

                if(abs(low$crit1 - b[6]) > 1){ # assuming ORCs

                        # Put into Secondary
                        secd_tab <- rbind(secd_tab, low)

                        prim_tab <- subset(prim_tab, rank != rk)

                } else {

                        # Pull Up
                        srk <- with(secd_tab, min(rank[side != xs]))
                        hgh <- secd_tab[secd_tab$rank == srk, ]

                        prim_tab <- rbind(prim_tab, hgh)

                        secd_tab <- subset(secd_tab, rank != srk)

                }

                ss <- countSide(prim_tab)

        }

        return(list(prim = prim_tab, secd = secd_tab))

}