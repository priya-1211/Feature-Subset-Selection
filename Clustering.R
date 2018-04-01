Clustering <- function(Tree) {
  forest <- list()
  Q <- vector()
  x <- 1
  j <- 1
  if(is.null(nrow(Tree))){
    nrow.Tree <- 0
  } else {
    nrow.Tree <- nrow(Tree)
  }
  while (nrow.Tree > 0) {
    temp1 <- vector()
    node <- Tree[1,1]
    temp1[j] <- node
    while (!is.na(node)) {
      index <- vector()
      index <- which(Tree[,1] == node, arr.ind = T)
      #i <- 1
      while (length(index) != 0) {
        if(length(which(Q == Tree[index[1],2])) == 0){
          Q  <- Enqueue(Q,Tree[index[1],2])}
        Tree[index[1],] <- NA
        index  <-  Dequeue(index)
      }
      node <- Q[1]
      j <- j + 1
      if(length(which(temp1 == node)) == 0){
        temp1[j] <- node}
      Q  <-  Dequeue(Q)
      ##flag <- (length(Q)!=0)
    }
    Tree <- Tree[complete.cases(Tree), ]
    temp1 <- temp1[complete.cases(temp1)]
    forest[[x]] <- temp1
    x <- x + 1
    if(is.null(nrow(Tree))){
      nrow.Tree <- 0
    } else {
      nrow.Tree <- nrow(Tree)
    }
  }
  return(forest)
}
