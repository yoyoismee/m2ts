#' genGreySeq
#' generate base object for others function
#' @author Pj.
#' @param valstep vector or numeric indicate level of each feature
#' @param sourceData orginal data of the model
#' @export

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

#' convert to ts
#' take model and basic sequence to create time series
#' @param gco a basic sequence
#' @param model a model
#' @export

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
