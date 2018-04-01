normlize.data <- function(x) {
  output <- matrix(data = NA ,
                   nrow = nrow(x),
                   ncol = ncol(x))
  for (j in 1:ncol(x)) {
    for (i in 1:nrow(x)) {
      output[i, j] <-
        (x[i, j] - min(x[, j], na.rm = TRUE)) / (max(x[, j], na.rm = TRUE) -
                                                   min(x[, j], na.rm = TRUE))
    }
  }
  return(output)
}
