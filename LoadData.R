LoadData <- function(dataset="D1"){
  
  if(dataset == "D1")
    {
    Dataset <- read.csv("E:/TO_upload/1.Datasets/D1_ALLAML.csv",row.names = NULL)
  }else if(dataset == "D2")
    {
    Dataset <- read.csv("E:/TO_upload/1.Datasets/D2_arcene.csv",row.names = NULL)  
  }else if(dataset == "D3")
    {
    Dataset <- read.csv("E:/TO_upload/1.Datasets/D3_GLI85.csv",row.names = NULL)
    Y<-Dataset$Y
    Dataset<-Dataset[,-2]
    Dataset<-cbind(Dataset,Y)
  }else if(dataset == "D4")
    {
    Dataset <- read.csv("E:/TO_upload/1.Datasets/D4_Prostate_GE.csv",row.names = NULL)
  }else if(dataset == "D5")
    {
    Dataset <- read.csv("E:/TO_upload/1.Datasets/D5_SMK_CAN_187.csv",row.names = NULL)
    Y<-Dataset$Y
    Dataset<-Dataset[,-2]
    Dataset<-cbind(Dataset,Y)
  }
  # rownames(Dataset) <- Dataset[,1]
  # Dataset<-Dataset[,-1]
  index<-which(colnames(Dataset)=="X")
  Dataset<-Dataset[,-index]
  for(i in 1:nrow(Dataset)){
    if(Dataset$Y[i]!=1){Dataset$Y[i]<-0}
  }
  return(Dataset)
}