genGreySeq<- function(valstep=9,minval=NULL,maxval=NULL,sourceData=NULL,style = "SD"){
  if(is.null(sourceData)&&(is.null(minval)||is.null(maxval))){
    stop("insufficient input")
  }
  if(!is.null(sourceData)&&(!is.null(minval)||!is.null(maxval))){
    warning("ignored minval and maxval")
  }
  if(!is.null(sourceData)){
    if(length(valstep)!=1){
      stop("invalid valstep")
    }
    if(style=="SD"){
      mn <- sapply(FUN = mean,X = sourceData)
      sdd <-sapply(FUN = sd,X = sourceData)
      if(valstep<4){
        maxval<- mn + sdd*1
        minval<- mn - sdd*1
      }else if(valstep<8){
        maxval<- mn + sdd*2
        minval<- mn - sdd*2
      }else{
        maxval<- mn + sdd*2.5
        minval<- mn - sdd*2.5
      }
    }else if(style=="range"){
      maxval <- sapply(FUN = max,X = sourceData)
      minval <- sapply(FUN = min,X = sourceData)
    }
    valstep <- rep(x = valstep,length(maxval))
  }
  if(length(valstep)!=1&&length(valstep)!=length(maxval)){
    stop("invalid valstep")
  }
##################################################
  x<-data.frame("x",1:valstep[1]-1)
  for(valn in 2:length(valstep)){
    tmp <- x
    x<-NULL
    for(val in 1:valstep[valn]){
      if(val%%2==0){
        y<-tmp[nrow(y):1,]
      }else{
        y<-tmp
      }
      y[,ncol(y)+1]<-val-1
      x<-rbind(x,y)
    }
  }
  names(x)<-NULL
  x<-x[,2:ncol(x)]
  rownames(x) <- c()
  if(!(is.null(minval)|is.null(maxval))){
    for(i in 1:length(valstep)){
      x[,i]<-minval[i]+x[,i]*(maxval[i]-minval[i])/(valstep[i]-1)
    }
  }
  #M2TSgco<-setClass("M2TSgco", slots = c(code="data.frame"))
  #gco<-M2TSgco(code = x)
  #gco
  x
}

convertTS <- function(gco,originalData,model=NULL,modelFunction=NULL,target=NULL,...){
  if(is.null(model)&is.null(modelFunction)){
    stop("model are missing")
  }
  if(!is.null(model)&!is.null(modelFunction)){
    warning("ignored modelFunction")  
  }
  if(!is.null(model)){
    names(gco)<- names(originalData)
    val<-predict(model,gco)
    data.frame(idx=1:length(val),val)
    
  }else{
    names(gco)<- names(originalData)
    val<-modelFunction(originalData,target,gco,list(...))
    data.frame(idx=1:length(val),val)
    
  }
}


testF <- function(traindata,target,testdata,k){
  #traindata<-cbind(traindata,target)
  knn(train = traindata,cl=target,test = testdata,k=k)
}
#gc<- genGreySeq(c(10,10),minval = c(-1,-1),maxval = c(1,1))


#df <- iris
#df$Species<- ifelse(df$Species=="setosa",0,ifelse(df$Species=="versicolor",1,2))\
dff <- read.csv("machine.csv")
eq <- PRP~.
gc<- genGreySeq(valstep = 2, sourceData = dff[,1:(length(dff)-1)],style = "SD")


dff <- read.csv("tic-tac-toe.csv")
eq <- res~.
gc<- genGreySeq(valstep = 2, sourceData = dff[,1:(length(dff)-1)],style = "range")

fit<- randomForest(eq,data = dff)
fit<- ctree(eq,data = dff)
fit<- lm(eq,data = dff)
fit<- svm(eq,data = dff)
fit<- nnet(dff[,1:(length(dff)-1)],dff[,length(dff)], size = 3, rang = 0.1,decay = 5e-4, maxit = 200)
out<-convertTS(gco = gc,model = fit,originalData = dff[,1:(length(dff)-1)])
plot(out,type = "l")

out<-convertTS(gco = gc,modelFunction = testF,originalData = dff[,1:(length(dff)-1)],k=5,target = dff[,length(dff)])
plot(out,type = "l")



