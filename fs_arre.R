## Clear workspace
rm(list = ls())
## Include library
library(roahd)
library(SDMTools)
library(R.matlab)
library(stats)
library(ica)
library(caret)
######################################################################################
## Function Declaration
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
Fisher <- function(Z,C){
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
    for (i in 1:nrow(target)) {
      if (target[i,1] == 1) {
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
fs_rredt <- function(dataset,is.normalized=TRUE) {
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
    tree.arcs.remove <- which(tree.arcs[,3]<0.5)
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
Classification.Results<-function(input,target,data){
fss <- suppressWarnings(fs_rredt(input))
print(dim(input))
print(length(fss))
data.fss <- remove.NA(input[,fss]) 
if(anyNA(input[,-fss])){
  data.remaining<-remove.NA(input[,-fss])}else{
    data.remaining<-input[,-fss]
  }
input.row.count = nrow(data.fss)
input.column.count = ncol(data.fss)
input<-data.fss
flag<-TRUE
while(flag){
  fss <- suppressWarnings(fs_rredt(input))
  if(length(fss) == input.column.count){
    flag<-FALSE}
  if(anyNA(input[,fss])){
    data.fss <- remove.NA(input[,fss])}else{
      data.fss <- input[,fss]
    }
  input<-data.fss
  input.column.count = ncol(input)
}
data.pca <- as.matrix(data.remaining)
pca <- prcomp(data.pca, scale=FALSE)
data.remaining.reduced <- predict(pca,data.pca)
pca1 <- data.remaining.reduced[,2]
pca1 <- round(pca1,2)
ica.data.cols <- ncol(data.remaining)
if(ica.data.cols > 2000){
  imod <- icaimax(data.remaining[,1:2000],2)
}else{
  imod <- icaimax(data.remaining[,1:ica.data.cols],2)
}
colnames(imod$S) <- c("ica1","ica2")
#rre.data<-data.fss
rredt.data <- cbind(data.fss,pca1,imod$S)
### Preparing Training and testing data
intrain <- createDataPartition(y = data$Y, p = 0.7, list = FALSE)
#training.data.rre<-rre.data[intrain,]
training.data <- rredt.data[intrain,]
training.class<-factor(data$Y[intrain])
#training.class.rre<- rre.data[-intrain,]
testing.data <- rredt.data[-intrain,]
testing.class <- data$Y[-intrain]
actual<-sapply(testing.class,FUN = function(x){if(x==1){x<-1}else{x<-0}})
#dim(training.data.rre);dim(training.class.rre);
dim(training.data); dim(testing.data);

## Method 1 : SVM Linear
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)
start.pt<-proc.time()
svm_Linear <- train(training.data,training.class,method = "svmLinear",
                    trControl=trctrl,
                    tuneLength = 10)
svm.linear.predicted <- predict(svm_Linear,testing.data)
svm.linear<-sapply(svm.linear.predicted,FUN = function(x){if(x==1){x<-1}else{x<-0}})
rredt.LSvm.cm<-confusion.matrix(actual,svm.linear)
rredt.lsvm.acc<-accuracy(actual,svm.linear)
time.rredt.lsvm<-proc.time()-start.pt
#####################################################################################
## Method 2 : SVM Radial
set.seed(3233)
start.pt<-proc.time()
svm_Radial <- train(training.data,training.class,method = "svmRadial",
                    trControl=trctrl,
                    tuneLength = 10)
svm.Radial.predicted <- predict(svm_Radial,testing.data)
svm.Radial<-sapply(svm.Radial.predicted,FUN = function(x){if(x==1){x<-1}else{x<-0}})
rredt.rsvm.cm<-confusion.matrix(actual,svm.Radial)
rredt.rsvm.acc<-accuracy(actual,svm.Radial)
time.rredt.Rsvm<-proc.time()-start.pt
#####################################################################################
## Method 3 : Knn Classification
set.seed(3233)
start.pt<-proc.time()
knn.train.model <- train(training.data,training.class,method = "knn",
                         trControl=trctrl,
                         tuneLength = 10)
knn.predicted <- predict(knn.train.model,testing.data)
knn<-sapply(knn.predicted,FUN = function(x){if(x==1){x<-1}else{x<-0}})
rredt.knn.cm<-confusion.matrix(actual,knn)
rredt.knn.acc<-accuracy(actual,knn)
time.rredt.knn<-proc.time()-start.pt
#####################################################################################
##  Method 4 :  Random Forest Classification 
set.seed(3233)
start.pt<-proc.time()
rf.train.model <- train(training.data,training.class,method = "rf",
                        trControl=trctrl,
                        tuneLength = 10)

rf.predicted <- predict(rf.train.model,testing.data)
RandomForest<-sapply(rf.predicted,FUN = function(x){if(x==1){x<-1}else{x<-0}})
rredt.rf.cm<-confusion.matrix(actual,RandomForest)
rredt.rf.acc<-accuracy(actual,RandomForest)
time.rredt.rf<-proc.time()-start.pt
######################################################################################
# summarize accuracy of models
print("Summary - Training Models")
results <- resamples(list(LSVM=svm_Linear, RSVM=svm_Radial, knn=knn.train.model, rf=rf.train.model))
result.summary<-summary(results)
result<-data.frame()
result[1,1]<-time.rredt.lsvm[1]
result[1,2]<-time.rredt.Rsvm[1]
result[1,3]<-time.rredt.knn[1]
result[1,4]<-time.rredt.rf[1]
result[2,1]<-rredt.lsvm.acc[1]
result[2,2]<-rredt.rsvm.acc[1]
result[2,3]<-rredt.knn.acc[1]
result[2,4]<-rredt.rf.acc[1]
result[3,1]<-rredt.lsvm.acc[2]
result[3,2]<-rredt.rsvm.acc[2]
result[3,3]<-rredt.knn.acc[2]
result[3,4]<-rredt.rf.acc[2]
result[4,1]<-rredt.lsvm.acc[3]
result[4,2]<-rredt.rsvm.acc[3]
result[4,3]<-rredt.knn.acc[3]
result[4,4]<-rredt.rf.acc[3]
result[5,1]<-rredt.lsvm.acc[4]
result[5,2]<-rredt.rsvm.acc[4]
result[5,3]<-rredt.knn.acc[4]
result[5,4]<-rredt.rf.acc[4]
result[6,1]<-rredt.lsvm.acc[5]
result[6,2]<-rredt.rsvm.acc[5]
result[6,3]<-rredt.knn.acc[5]
result[6,4]<-rredt.rf.acc[5]
result[7,1]<-rredt.lsvm.acc[6]
result[7,2]<-rredt.rsvm.acc[6]
result[7,3]<-rredt.knn.acc[6]
result[7,4]<-rredt.rf.acc[6]
result[8,1]<-rredt.lsvm.acc[7]
result[8,2]<-rredt.rsvm.acc[7]
result[8,3]<-rredt.knn.acc[7]
result[8,4]<-rredt.rf.acc[7]
result[9,1]<-rredt.LSvm.cm[1]
result[10,1]<-rredt.LSvm.cm[3]
result[11,1]<-rredt.LSvm.cm[2]
result[12,1]<-rredt.LSvm.cm[4]

result[9,2]<-rredt.rsvm.cm[1]
result[10,2]<-rredt.rsvm.cm[3]
result[11,2]<-rredt.rsvm.cm[2]
result[12,2]<-rredt.rsvm.cm[4]

result[9,3]<-rredt.knn.cm[1]
result[10,3]<-rredt.knn.cm[3]
result[11,3]<-rredt.knn.cm[2]
result[12,3]<-rredt.knn.cm[4]

result[9,4]<-rredt.rf.cm[1]
result[10,4]<-rredt.rf.cm[3]
result[11,4]<-rredt.rf.cm[2]
result[12,4]<-rredt.rf.cm[4]
rownames(result)<-c("proc.time","threshold","AUC","omission rate","sensitivity","specificity","prop.correct","Kappa","True-Positive","True-negetive","False-Positive","False-negative")
return(result)
}
####################################################################################
print("ALLAML")
## Dataset 1: ALLAML
ALLAML<-readMat("F:/DATASET/ALLAML.mat")
ALLAML<-as.data.frame(ALLAML)
input<-ALLAML[,1:7129]
target<-as.data.frame(ALLAML[,7130])
data<-ALLAML
classification<-Classification.Results(input,target,data)
dd1<-classification
print("arcene")
## Dataset 2: arcene
arcene<-readMat("F:/DATASET/arcene.mat")
arcene<-as.data.frame(arcene)
input<-arcene[,1:10000]
target<-as.data.frame(arcene[,10001])
data<-arcene
classification<-Classification.Results(input,target,data)
dd2<-classification
print("madelon")
## Dataset 3: madelon
madelon<-readMat("F:/DATASET/madelon.mat")
madelon<-as.data.frame(madelon)
input<-madelon[,2:501]
target<-as.data.frame(madelon[,1])
data<-madelon
classification<-Classification.Results(input,target,data)
dd3<-classification
print("GLI85")
## Dataset 4: GLI-85
GLI85<-readMat("F:/DATASET/GLI_85.mat")
GLI85<-as.data.frame(GLI85)
input<-GLI85[,2:22284]
target<-as.data.frame(GLI85[,1])
data<-GLI85
classification<-Classification.Results(input,target,data)
dd4<-classification
View(d4)
print("d5")
## Dataset 5: Prostate_GE
Prostate_GE<-readMat("F:/DATASET/Prostate_GE.mat")
Prostate_GE<-as.data.frame(Prostate_GE)
input<-Prostate_GE[,1:5966]
target<-as.data.frame(Prostate_GE[,5967])
data<-Prostate_GE
classification<-Classification.Results(input,target,data)
dd5<-classification
View(dd5)
print("SMK_CAN_187")
## Dataset 6: SMK_CAN_187
SMK_CAN_187<-readMat("F:/DATASET/SMK_CAN_187.mat")
SMK_CAN_187<-as.data.frame(SMK_CAN_187)
input<-SMK_CAN_187[,2:19994]
target<-as.data.frame(SMK_CAN_187[,1])
data<-SMK_CAN_187
classification<-Classification.Results(input,target,data)
dd6<-classification
View(dd6)
