clean <- function(dat){
  x <- dat$amta
  y <- dat$wpb
  
  cols <- intersect(colnames(x), colnames(y))
  fold <- cbind(rbind(cbind(x[, cols], type = 'amta'), 
                      cbind(y[ ,cols], type = 'wpb')))
  
  return(fold)
}