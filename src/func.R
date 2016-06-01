#################### 特征筛选
subsetVars<- function(trainingdata) {
  ########## del near zero var
  trainingdata<-as.data.frame(trainingdata)
  #  cat("##########特征筛选前维度:",dim(trainingdata),"\n")
  
  dimbak<-dim(trainingdata)[2]
  #fpCols<- !grepl("geoid", names(trainingdata))
  #trainingdata<-trainingdata[,fpCols]
  
  nzv <- nearZeroVar(trainingdata, saveMetrics = TRUE)
  trainingdata <- trainingdata[, !nzv$zeroVar]
  #  trainingdata <- trainingdata[, !nzv$nzv]
  #system.time(trainingdata = trainingdata[, -nearZeroVar(trainingdata)])
  # cat("########## nearZeroVar:删除近零特征数:",dimbak-dim(trainingdata)[2]," cloms\n")
  #  cat("########## 保留特征有 :\n")
  #  print(names(trainingdata))
  dimbak<-dim(trainingdata)[2]
  ######### del linearcombos
  #  cat("################ findLinearCombos:去线性相关的列(所剩特征不多,代码注掉了):\n")
  
  #comboInfo <- findLinearCombos(trainingdata)
  #if(is.null(comboInfo$remove))
  #{
  #print(system.time(trainingdata <- trainingdata[, -comboInfo$remove]))
  #    cat("findLinearCombos s:",comboInfo$remove,"\n")
  #  }
  
  #  cat("################ findCorrelation:特征相似性计算,去掉相似特征(所剩特征不多,代码住掉了..):\n")
  
  #correlations=cor(t)
  #  dim(correlations)
  #  write.table(correlations,"correlations",sep="\t",quote=FALSE,row.names=FALSE)
  
  #corrplot(correlations, order = "hclust")
  #  highCorr <- findCorrelation(correlations, cutoff = .90)
  #  if(!is.null(highCorr))
  #  {
  
  #   trainingdata <- trainingdata[, -highCorr]
  #    cat(" findCorrelation:",highCorr,"\n")
  #  }
  return (trainingdata)
}



######################## 数值型数据  转成近似正态数据   boxcoxtrans 这个方法很赞
preNumDatafucYestodayImp <- function(data,inf,getpr) {
  if(getpr==1){
    inf<-list()
    data[data==-1]<--9999
    
    inf$inf <- BoxCoxTrans(data[data>0],na.rm=TRUE)
    inf$median <- median(data[data!=-9999])
    inf$min <- min(data[data!=-9999])
    return(inf)
  }
  else{
    if(length(data[data==-1])!=0)  
      data[data==-1]<--9999
    if(length(data[data>0])!=0)
      data[data>0]<-round(predict(inf$inf, data[data>0]),3)
    if(length(data[data==-9999])!=0)  
      data[data==-9999]<-inf$min-(inf$median-inf$min)*0.1
    return (data)
  }
}

##############################得到测试集时间
get_tr_day_mid <- function() {
  date<-c("2016-01-01","2016-01-02","2016-01-03","2016-01-04","2016-01-05","2016-01-06","2016-01-07","2016-01-08","2016-01-09","2016-01-10","2016-01-11","2016-01-12","2016-01-13","2016-01-14","2016-01-15","2016-01-16","2016-01-17","2016-01-18","2016-01-19","2016-01-20","2016-01-21","2016-01-22","2016-01-24","2016-01-26","2016-01-28","2016-01-30")
  mid<-as.character(c(1:66))
  date_mid<-NULL
  for(m in date)
  {
    for(n in mid)
    {
      date_mid<-rbind(date_mid,paste(m,n,sep = ":"))    
    }
  }
  date_mid<-as.data.frame(date_mid)
  names(date_mid)<-"day_mid"
  return (date_mid)
}


#############################得到测试集时间
get_te_day_mid <- function() {
  date<-c("2016-01-22","2016-01-24","2016-01-26","2016-01-28","2016-01-30")
  mid<-as.character(c(1:66))
  date_mid<-NULL
  for(m in date)
  {
    for(n in mid)
    {
      date_mid<-rbind(date_mid,paste(m,n,sep = ":"))    
    }
  }
  date_mid<-as.data.frame(date_mid)
  names(date_mid)<-"day_mid"
  return (date_mid)
}


########################### 分类型变量 转成多列
datadummyVarsbuy <- function(trainingdata) {  
  trainingdata$week<-as.character(trainingdata$week)
  
  dummies <- dummyVars(~ ., data = trainingdata)
  
  system.time((trainingdata<-predict(dummies, newdata = trainingdata)))
  trainingdata<-as.data.frame(trainingdata) 
  return (trainingdata)
}

########################################################################################
###模型                        part I  线性模型














