Fisher <- function(Z,target){
  F <- matrix()
  #   F(Z)  <-  Fisher Score
  #   Z     <-  Data Space
  #   Sb    <-  Between Class scatter matrix
  #   St    <-  Total Scatter matrix
  #   gamma <-  positive regulation constraint
  #   I     <-  identity matrix
  #   F(Z)  <-  Sb/(St+gamma*I)
  #   Sb    <-  SumOf(nk(mean_k-mean)*transpose(mean_k-mean))
  #   St    <-  Sumof (fsilon-mean)*transpose(fsilon-mean)
  #   nk    <-  Size of k th class
  #   mean  <-  Sumof(nk*mean_k)
  #   fsilon<-  mean(i th feature vector)
  nc <- ncol(Z)
  for (c in 1:nc) {
    n0 <- 0
    n1 <- 0
    mea1 <- 0
    mea0 <- 0
    #calculate nk
    for (i in 1:length(target)) {
      if (target[i] == 1) {
        mea1 <- mea1 + Z[i,c]
        n1 <- n1 + 1
      } else {
        n0 <- n0 + 1
        mea0 <- mea0 + Z[i,c]
      }
    }
    # if class is 1 then nk=n1 otherwise nk=n0
    fsilon <- mean(Z[,c])
    mea <- (mea0 + mea1)/2
    X0 <- round(mea - mea0,4)
    Y0 <- round(t(X0),4)
    X1 <- round(mea - mea1,4)
    Y1 <- round(t(X1),4)
    Sb <- round(sum((n0*X0*Y0),(n1*X1*Y1)),4)
    eX0 <- round(fsilon - mea0,4)
    eY0 <- round(t(eX0),4)
    eX1 <- round(fsilon - mea1,4)
    eY1 <- round(t(eX1),4)
    St <- round(sum(eX0*eY0,eX1*eY1),4)
    gamma <- 1
    #nr<-nrow(St)
    #nc<-ncol(St)
    #I<-diag(nr )
    #F<-Sb/(St+gamma*I)
    F[c] <- signif(Sb/St,3)
  }
  return(F)
}
