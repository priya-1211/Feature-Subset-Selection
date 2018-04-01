Dequeue <- function(Q) {
  Qindex <- length(Q)
  if (Qindex == 0)
  {
    Q <- vector()
  }else if (Qindex == 1)
  {
    Qindex <- Qindex - 1
    Q <- vector()
  }else
  {
    Qindex <- Qindex - 1
    Q <- Q[2:length(Q)]
  }
  return(Q)
}
