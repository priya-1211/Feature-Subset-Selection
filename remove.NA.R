remove.NA <- function(x) {
  na.index <- which(is.na(x),arr.ind = TRUE)
  na.row<-nrow(na.index)
  if(length(na.index)){
    for(i in 1:na.row) {
      x[na.index[i,1],na.index[i,2]] <- mean(x[, na.index[i,2]], na.rm = TRUE)
    }
  }
  return(x)
}
