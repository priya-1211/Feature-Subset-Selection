  library(kernlab)
  data<-cbind(D1.RRE.result[[3]],D1$Y)
  intrain <- createDataPartition(y = D1$Y, p = 0.7, list = FALSE)
  #training.data.rre<-rre.data[intrain,]
  training.data <- data[intrain,]
  training.class<-factor(D1$Y[intrain])
  #training.class.rre<- rre.data[-intrain,]
  testing.data <- data[-intrain,]
  testing.class <- D1$Y[-intrain]
  # Kernel 1 <- linear
  actual<-sapply(testing.class,FUN = function(x){if(x==1){x<-1}else{x<-0}})
  k.linear <- ksvm(D1$Y[intrain]~.,data=data[intrain,],kernel = "vanilladot",cross=10)
  predict.linear<-predict(k.linear,testing.data)
  class.linear<-sapply(predict.linear,FUN = function(x){if(x<=1.5){x<-1}else{x<-0}})
  linear.cm<-confusion.matrix(actual,class.linear)
  linear.acc<-accuracy(actual,class.linear)
  #Kernel 2 = polydot
  k.poly <- ksvm(D1$Y[intrain]~.,data=data[intrain,],kernel = "polydot",cross=10)
  k2<-k.poly@fitted
  predict.poly<-predict(k.poly,testing.data)
  class.poly<-sapply(predict.poly,FUN = function(x){if(x<=1.5){x<-1}else{x<-0}})
  poly.cm<-confusion.matrix(actual,class.poly)
  poly.acc<-accuracy(actual,class.poly)
  #Kernel 3 = polydot of degree 3
  k.polypoly <- ksvm(D1$Y[intrain]~.,data=data[intrain,],kernel = "polydot",kpar=list(degree=3),cross=10)
  k3<-k.polypoly@fitted
  predict.polypoly<-predict(k.polypoly,testing.data)
  class.polypoly<-sapply(predict.polypoly,FUN = function(x){if(x<=1.5){x<-1}else{x<-0}})
  polypoly.cm<-confusion.matrix(actual,class.polypoly)
  polypoly.acc<-accuracy(actual,class.polypoly)
  #Kernel 4 = Anova Rbf
  k.anova <- ksvm(D1$Y[intrain]~.,data=data[intrain,],kernel = "anovadot",cross=10)
  k4<-k.linear@fitted
  predict.anova<-predict(k.anova,testing.data)
  class.anova<-sapply(predict.anova,FUN = function(x){if(x<=1.5){x<-1}else{x<-0}})
  anova.cm<-confusion.matrix(actual,class.anova)
  anova.acc<-accuracy(actual,class.anova)
  max(anova.acc$prop.correct,polypoly.acc$prop.correct,poly.acc$prop.correct,linear.acc$prop.correct)
obj<-min(k.linear@obj,k.poly@obj,k.polypoly@obj,k.anova@obj)
  if(obj==k.anova@obj){
    new.Kernal<-"anovadot"
  }else if(obj==k.polypoly@obj){
    new.Kernal<-"polydot"
    kpar=list(degree=3)
  }else if(obj==k.poly@obj){
    new.Kernal<-"polydot"
    kpar=list(degree=1)
  }else if(obj==k.linear@obj){
    new.Kernal<-"vanilladot"
  }
class(new.Kernal)="kernal"
k.hsvm <- ksvm(D1$Y[intrain]~.,data=data[intrain,],kernel = new.Kernal,cross=10)
predict.hsvm<-predict(k.hsvm,testing.data)
class.hsvm<-sapply(predict.hsvm,FUN = function(x){if(x<=1.5){x<-1}else{x<-0}})
hsvm.cm<-confusion.matrix(actual,class.hsvm)
hsvm.acc<-accuracy(actual,class.hsvm)
