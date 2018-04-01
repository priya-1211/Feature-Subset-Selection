fss_RRE <- function(dataset,is.normalized=TRUE,corr.threshold=0.7) {
  ## initialization
  fss <- vector()
  start.index = 1
  ## Extract a small grid from dataset
  input.row.count = nrow(dataset)
  input.column.count = ncol(dataset)
  temp <- input.column.count
  if (temp < floor(input.row.count/2)) {
    Grid.size <- temp
  }else {
    Grid.size <- floor(input.row.count/2)
    temp <- temp - Grid.size
  }
  stop.index <- start.index + Grid.size - 1
  while (stop.index <= input.column.count) {
    
    if(stop.index > start.index){
      Grid.data <- dataset[, start.index:stop.index]
      ## Remove NA values and replace with mean column value
      if(anyNA(Grid.data)){
        data.withoutNA <- remove.NA(Grid.data)}else{
          data.withoutNA <- Grid.data
        }
      ##Function to normalize data in 0-1 range
      if(is.normalized){data.normalized<-data.withoutNA}else{
        data.normalized <- normlize.data(data.withoutNA)}
      CostMatrix <- abs(cor(data.normalized))
      CostMatrix.row <- nrow(CostMatrix)
      Graph.arcs <- matrix(ncol = 3)[-1, ]
      for (i in 1:CostMatrix.row) {
        for (j in 1:i) {
          if (CostMatrix[i,j] != 0 & CostMatrix[i,j] != Inf & !is.na(CostMatrix[i,j])) {
            Graph.arcs <- rbind(Graph.arcs,cbind(i+start.index-1,j+start.index-1,CostMatrix[i,j]))
          }
        }
      }
      nodes <- start.index:stop.index
      ## Construct a Maximum Spanning Tree
      tree.arcs<-msTreePrim(nodes,Graph.arcs,start.index)
      ## removhe the arc with minimum cost
      tree.arcs.remove <- which(tree.arcs[,3]<corr.threshold)
      if(length(tree.arcs.remove) != 0){
        tree.arcs <- tree.arcs[-c(tree.arcs.remove), ]
        ## Cluster the data
        Clusters <- Clustering(tree.arcs)}else{
          Clusters <- list()
          Clusters[[1]] <- start.index:stop.index
        }
      ## Evaluate Fisher Score for input subset
      cluster.count <- length(Clusters)
      if(cluster.count>0){
        for (i in 1:cluster.count) {
          data <- data.normalized[,sapply(c(Clusters[[i]]),FUN=function(x){x<-x-start.index+1;return(x)})]
          Target <- target[sapply(c(Clusters[[i]]),FUN=function(x){x<-x-start.index+1;return(x)}),1]
          Fisher_score <- round(Fisher(data,Target),3)
          feature.selected <- which(Fisher_score == max(Fisher_score))
          #fss <- cbind(fss,Clusters[[i]][feature.selected])
          fss<-append(fss, Clusters[[i]][feature.selected], after = length(fss))
        }
      }
      start.index <- stop.index + 1
      if (temp < floor(input.row.count/2)){
        Grid.size <- temp}else {
          Grid.size <- floor(input.row.count/2)
          temp <- temp - Grid.size
        }
      stop.index <- start.index + Grid.size - 1
    }else{
      stop.index <- input.column.count+1
    }
    
  }
  return(fss)
}
