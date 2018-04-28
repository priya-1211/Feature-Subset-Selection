##  Execute different feature selection algorithms using the function fss_Algorithms
fss_Algorithms <- function(data,Fss_Alg=c("FCBF","CFS","Fisher","mRMRe","ReliefF","RRE"),corr.threshold=0.6){
  library(Biocomb)
  # source('E:/TO_upload/3.Implementation/SourceFunctions.R')
  # SourceFunctions()
  x<-data[,-ncol(data)]
  y<-data$Y
  attrs.nominal=ncol(data)
  data[,ncol(data)]<-as.factor(data[,ncol(data)])
  if(Fss_Alg == "FCBF")
    {
    # FCBF -
    set.seed(3233)
    start.pt<-proc.time()
       
     fss.fcbf<-select.fast.filter(data,disc.method="MDL",threshold=0,attrs.nominal=numeric())
        fcbf.data<-data[,c(fss.fcbf[,3])]
        train <- createDataPartition(y,p = 0.7, list = FALSE)
        training.data <- fcbf.data[train,]
        training.class<-factor(data$Y[train])
        testing.data <- fcbf.data[-train,]
        testing.class <- data$Y[-train]
        actual<-sapply(testing.class,FUN = function(x){if(x==1){x<-1}else{x<-0}})
        trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
        rsvm.train.model <- train(training.data,training.class,method = "svmRadial",
                                 trControl=trctrl,
                                 tuneLength = 10)
        rsvm.predicted <- predict(rsvm.train.model,testing.data)
        rsvm<-sapply(rsvm.predicted,FUN = function(x){if(x==1){x<-1}else{x<-0}})
        fcbf.rsvm.acc<-accuracy(actual,rsvm)
        time.fcbf<-proc.time()-start.pt
        return(list(length(fss.fcbf[,3]),time.fcbf,fcbf.rsvm.acc,fcbf.data))
        
  }else if(Fss_Alg == "CFS")
    {
    # CFS
    set.seed(3233)
    start.pt<-proc.time()
    # if(ncol(data)>7500){
    #   data1<-data[,1:(ncol(data)/2)]
    #   data1<-cbind(data1,as.factor(y))
    #   fss.cfs<-select.forward.Corr(data1,disc.method="MDL",attrs.nominal=numeric())
    #   data2<-data[,1+(ncol(data)/2):ncol(data)]
    #   fss.cfs1<-select.forward.Corr(data2,disc.method="MDL",attrs.nominal=numeric())
    #   fss.cfs<-append(fss.cfs,fss.cfs1)  
    # }else{
      fss.cfs<-select.forward.Corr(data,disc.method="MDL",attrs.nominal=numeric()) 
    # }
    cfs.data<-data[,c(fss.cfs)]
    train <- createDataPartition(y,p = 0.7, list = FALSE)
    training.data <- cfs.data[train,]
    training.class<-factor(data$Y[train])
    testing.data <- cfs.data[-train,]
    testing.class <- data$Y[-train]
    actual<-sapply(testing.class,FUN = function(x){if(x==1){x<-1}else{x<-0}})
    trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
    rsvm.train.model <- train(training.data,training.class,method = "svmRadial",
                             trControl=trctrl,
                             tuneLength = 10)
    rsvm.predicted <- predict(rsvm.train.model,testing.data)
    rsvm<-sapply(rsvm.predicted,FUN = function(x){if(x==1){x<-1}else{x<-0}})
    cfs.rsvm.acc<-accuracy(actual,rsvm)
    return(cfs.rsvm.acc)
  }else if(Fss_Alg == "Fisher")
    {
      target<-y
      Fisher_score <- round(Fisher(x,target),3)
      fss<-vector()
      fisher.data<-data.frame()
      fisher.data[1:nrow(data),1]<-NA
      fisher.rsvm.acc<-data.frame()
      i<-1
      FeatureCount<-0
      while(length(Fisher_score)>=1){
        feature.selected <- which(Fisher_score == max(Fisher_score, na.rm = TRUE))
        FeatureCount<-FeatureCount+length(feature.selected)
        Fisher_score<-Fisher_score[-c(feature.selected)]
        fisher.data<-cbind(fisher.data,x[,c(feature.selected)])
        fisher.data<-fisher.data[, colSums(is.na(fisher.data)) != nrow(fisher.data)]
        if(FeatureCount>1){
        train <- createDataPartition(y,p = 0.7, list = FALSE)
        train<-as.vector(train)
        training.data <- fisher.data[train,]
        training.class<-factor(data$Y[train])
        testing.data <- fisher.data[-train,]
        testing.class <- data$Y[-train]
        actual<-sapply(testing.class,FUN = function(x){if(x==1){x<-1}else{x<-0}})
        trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
        rsvm.train.model <- train(training.data,training.class,method = "svmRadial",
                                 trControl=trctrl,
                                 tuneLength = 10)
        rsvm.predicted <- predict(rsvm.train.model,testing.data)
        rsvm<-sapply(rsvm.predicted,FUN = function(x){if(x==1){x<-1}else{x<-0}})
        fisher.acc<-accuracy(actual,rsvm)
        fisher.rsvm.acc[i,1]<-FeatureCount
        fisher.rsvm.acc[i,2]<-fisher.acc$prop.correct
        i<-i+1
        }
      }
      plot(fisher.rsvm.acc[,1],fisher.rsvm.acc[,2],main="Fisher Feature Selection",xlab="No of Features",ylab="Accuracy(0-1)scale",lwd=3,type="l")
      index<-which(fisher.rsvm.acc[,2]==max(fisher.rsvm.acc[,2]))
      fisher.rsvm.acc[index,]
      return(fisher.rsvm.acc)
  }else if(Fss_Alg == "mRMRe")
    {
    # MRMR
    install.packages("mRMRe")
    library(mRMRe)
    data1 <- data.matrix(data, rownames.force = NA)
    data1[,1:ncol(data)] <- sapply(data[,1:ncol(data)], as.numeric)
    data1 <- mRMR.data(data = data.frame(data1))
    MrmrE<-mRMR.ensemble(data = data1, target_indices = c(ncol(data)),solution_count = 1, feature_count = 30)
    fss.mRMRe<-MrmrE@filters[[as.character(ncol(data))]]
    fss.mRMRe<-as.vector(fss.mRMRe)
    mRMRe.data<-data[,c(fss.mRMRe)]
    train <- createDataPartition(y,p = 0.7, list = FALSE)
    training.data <- mRMRe.data[train,]
    training.class<-factor(data$Y[train])
    testing.data <- mRMRe.data[-train,]
    testing.class <- data$Y[-train]
    actual<-sapply(testing.class,FUN = function(x){if(x==1){x<-1}else{x<-0}})
    trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
    rsvm.train.model <- train(training.data,training.class,method = "svmRadial",
                             trControl=trctrl,
                             tuneLength = 10)
    rsvm.predicted <- predict(rsvm.train.model,testing.data)
    rsvm<-sapply(rsvm.predicted,FUN = function(x){if(x==1){x<-1}else{x<-0}})
    mRMRe.rsvm.cm<-confusion.matrix(actual,rsvm)
    mRMRe.rsvm.acc<-accuracy(actual,rsvm)
    return(mRMRe.rsvm.acc)
    
  }else if(Fss_Alg == "ReliefF")
    {
    if(ncol(data)>7000){
      data1<-data[,1:2500]
      fssRelief<-select.relief(data1)
      data2<-data[,2501:ncol(data)]
      fssRelief1<-select.relief(data2)
    }else{
      fssRelief<-select.relief(data)
    }
     
     selected.features<-fssRelief[,3]
     Relief.rsvm.acc<-data.frame()
     for(i in 1:7){
       Relief.rsvm.acc[1,i]<-NA
     }
     Relief.rsvm.acc[]
    for(i in 2:length(selected.features)){
      Relief.data<-data[,c(selected.features[1:i])]
      train <- createDataPartition(y,p = 0.7, list = FALSE)
      training.data <- Relief.data[train,]
      training.class<-factor(data$Y[train])
      testing.data <- Relief.data[-train,]
      testing.class <- data$Y[-train]
      actual<-sapply(testing.class,FUN = function(x){if(x==1){x<-1}else{x<-0}})
      trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      rsvm.train.model <- train(training.data,training.class,method = "svmRadial",
                               trControl=trctrl,
                               tuneLength = 10)
      rsvm.predicted <- predict(rsvm.train.model,testing.data)
      rsvm<-sapply(rsvm.predicted,FUN = function(x){if(x==1){x<-1}else{x<-0}})
      Relief.rsvm.acc[i-1,]<-accuracy(actual,rsvm)
    }
     acc.max<-which(Relief.rsvm.acc[,6]==max(Relief.rsvm.acc[,6]))
     index<-seq(2:acc.max[1]+10)
     plot(index,Relief.rsvm.acc[(1:length(index)),6],main="Relief Feature Selection",xlab="No of Features",ylab="Accuracy(0-1)scale",lwd=3,type = "l")
     return(list(Relief.rsvm.acc,acc.max))
   }else if(Fss_Alg == "RRE")
     {
    fss_RRE <- function(dataset,target,is.normalized=TRUE,corr.threshold=0.95) {
      ## initialization
      target<-target
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
              Target <- target[sapply(c(Clusters[[i]]),FUN=function(x){x<-x-start.index+1;return(x)})]
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
    target<-y
    fss.RRE<-fss_RRE(x,y)
    RRE.data<-data[,c(fss.RRE)]
    train <- createDataPartition(y,p = 0.7, list = FALSE)
    training.data <- RRE.data[train,]
    training.class<-factor(data$Y[train])
    testing.data <- RRE.data[-train,]
    testing.class <- data$Y[-train]
    actual<-sapply(testing.class,FUN = function(x){if(x==1){x<-1}else{x<-0}})
    trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
    rsvm.train.model <- train(training.data,training.class,method = "svmRadial",
                             trControl=trctrl,
                             tuneLength = 10)
    rsvm.predicted <- predict(rsvm.train.model,testing.data)
    rsvm<-sapply(rsvm.predicted,FUN = function(x){if(x==1){x<-1}else{x<-0}})
    RRE.rsvm.cm<-confusion.matrix(actual,rsvm)
    RRE.rsvm.acc<-accuracy(actual,rsvm)
    return(list(RRE.rsvm.acc,fss.RRE,RRE.data))
  }
}
  





