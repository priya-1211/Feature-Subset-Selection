msTreePrim <- function(nodes, arcs, start.node) {
  arcs <- rbind(arcs, matrix(c(arcs[, 2], arcs[, 1], arcs[,3]), ncol = 3))
  tree.arcs <- matrix(ncol = 3)[-1, ]
  tree.nodes <- nodes[nodes == start.node]
  flag <- TRUE
  while (length(tree.nodes) < length(nodes) & flag) {
    k <- which(arcs[, 1] %in% tree.nodes & arcs[, 2] %in%
                 nodes[-which(nodes %in% tree.nodes)])
    validArcs <- matrix(arcs[k, ], ncol = 3)
    l <- which(validArcs[, 3] == suppressWarnings(max(validArcs[, 3])))
    max.arc <- matrix(validArcs[l, ], ncol = 3)
    if (length(max.arc) > 0) {
      tree.arcs <- rbind(tree.arcs, max.arc)
      tree.nodes <- c(tree.nodes, max.arc[1, 2])
    } else {
      flag <- nrow(max.arc)
    }
  }
  return(tree.arcs)
}
