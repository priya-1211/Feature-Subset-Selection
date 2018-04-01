Enqueue <- function(Q, element) {
  Qindex <- length(Q)
  if (Qindex == 0)
  {
    Qindex <- 1
    Q[Qindex] <- element
  }else
  {
    Qindex <- Qindex + 1
    Q[Qindex] <- element
  }
  return(Q)
}
