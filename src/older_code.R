#####################  第一版数据是用这个版本跑的


library(caret)

library(data.table)
library(AppliedPredictiveModeling)
library(glmnet)
library(MASS)
library(e1071)
library(kernlab)

library(frbs)
library(xgboost)



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


datadummyVars <- function(trainingdata) {  
  trainingdata$cate<-as.character(trainingdata$map_block)
  
  dummies <- dummyVars(~ ., data = trainingdata)
  
  system.time((trainingdata<-predict(dummies, newdata = trainingdata)))
  trainingdata<-as.data.frame(trainingdata) 
  return (trainingdata)
}

subsetVars<- function(trainingdata) {
  ########## del near zero var
  trainingdata<-as.data.frame(trainingdata)
  cat("################before del nearZeroVar  dim:",dim(trainingdata),"\n")
  
  dimbak<-dim(trainingdata)[2]
  #fpCols<- !grepl("geoid", names(trainingdata))
  #trainingdata<-trainingdata[,fpCols]
  
  nzv <- nearZeroVar(trainingdata, saveMetrics = TRUE)
  trainingdata <- trainingdata[, !nzv$nzv]
  #system.time(trainingdata <- trainingdata[, -nearZeroVar(trainingdata)])
  cat("################ nearZeroVar fuc del:",dimbak-dim(trainingdata)[2]," cloms\n")
  cat("################ now names:\n")
  print(names(trainingdata))
  dimbak<-dim(trainingdata)[2]
  ######### del linearcombos
  cat("################ findLinearCombos:\n")
  
  comboInfo <- findLinearCombos(trainingdata)
  if(is.null(comboInfo$remove))
  {
    #print(system.time(trainingdata <- trainingdata[, -comboInfo$remove]))
    cat("findLinearCombos s:",comboInfo$remove,"\n")
  }
  
  cat("################ findCorrelation:\n")
  
  correlations<-cor(trainingdata)
  dim(correlations)
  write.table(correlations,"correlations",sep="\t",quote=FALSE,row.names=FALSE)
  
  #corrplot(correlations, order = "hclust")
  highCorr <- findCorrelation(correlations, cutoff = .90)
  if(!is.null(highCorr))
  {
    
    trainingdata <- trainingdata[, -highCorr]
    cat(" findCorrelation:",highCorr,"\n")
  }
  gc()
  return (trainingdata)
}

pro_day<-function(data,dayv,flag){
  data<-as.data.table(data)
  if(flag==1)
  {
    for(k in c("2016-01-02","2016-01-03","2016-01-04","2016-01-05","2016-01-06","2016-01-07","2016-01-08","2016-01-09","2016-01-10","2016-01-11","2016-01-12","2016-01-13","2016-01-14","2016-01-15","2016-01-16","2016-01-17","2016-01-18","2016-01-19","2016-01-20","2016-01-21","2016-01-22","2016-01-24","2016-01-26","2016-01-28","2016-01-30")){ 
      v<-dayv[which(!(day!=k)),]$v1
      
      data[which(!(day!=k)),]<-data[which(!(day!=k)),]*v
    }
  }
  else
  {
    for(k in c("2016-01-02","2016-01-03","2016-01-04","2016-01-05","2016-01-06","2016-01-07","2016-01-08","2016-01-09","2016-01-10","2016-01-11","2016-01-12","2016-01-13","2016-01-14","2016-01-15","2016-01-16","2016-01-17","2016-01-18","2016-01-19","2016-01-20","2016-01-21","2016-01-22","2016-01-24","2016-01-26","2016-01-28","2016-01-30")){ 
      v<-dayv[which(!(day!=k)),]$v2
      
      data[which(!(day!=k)),]<-data[which(!(day!=k)),]/v
    }
  }
  return (data)
}


#每个时间片单独调  data 是训练集合,prdata是测试集   i 是哪个时间段的数据
buildtestdata<-function(data,prdata,i){
  
  alldata<-data[which(!(model_choose!=i)),]
  
  tebak<-alldata[which(!(day=="2016-01-01"|day=="2016-01-02"|day=="2016-01-03"|day=="2016-01-04"|day=="2016-01-05"|day=="2016-01-06"|day=="2016-01-07"|day=="2016-01-08"|day=="2016-01-09"|day=="2016-01-10"|day=="2016-01-11"|day=="2016-01-12"|day=="2016-01-13"|day=="2016-01-14"|day=="2016-01-15"|day=="2016-01-16" |day=="2016-01-17"|day=="2016-01-18"|day=="2016-01-19"|day=="2016-01-20"|day=="2016-01-21")),]
  tebak<-as.data.frame(tebak)
  
  alldata<-as.data.frame(alldata)
  prdata<-as.data.frame(prtestingdata[which(!(model_choose!=i)),])
  prdatabak<-prdata
  
  fild_bak<-alldata[,1:4]
  
  
  for(j in c("a1","a2","a3","a4","b1","b2","b3","b4","c1","c2","c3","c4")){
    inf<-preNumDatafucYestodayImp(alldata[,j],getpr=1)
    alldata[,j]<-preNumDatafucYestodayImp(alldata[,j],inf=inf,getpr=0)
    prdata[,j]<-preNumDatafucYestodayImp(prdata[,j],inf=inf,getpr=0)
  }
  
  
  #trainingdata<-datadummyVars(trainingdata)
  #testingdata<-datadummyVars(testingdata)
  
  #alldata<-subsetVars(alldata)
  
  #alldata<-datadummyVars(alldata)
  
  alldata<-as.data.table(alldata)
  
  system.time(datalist$tr[[i]]<-alldata[which(!(day=="2016-01-01"|day=="2016-01-22"|day=="2016-01-24"|day=="2016-01-26"|day=="2016-01-28"|day=="2016-01-30")),])
  system.time(datalist$te[[i]]<-alldata[which(!(day=="2016-01-01"|day=="2016-01-02"|day=="2016-01-03"|day=="2016-01-04"|day=="2016-01-05"|day=="2016-01-06"|day=="2016-01-07"|day=="2016-01-08"|day=="2016-01-09"|day=="2016-01-10"|day=="2016-01-11"|day=="2016-01-12"|day=="2016-01-13"|day=="2016-01-14"|day=="2016-01-15"|day=="2016-01-16" |day=="2016-01-17"|day=="2016-01-18"|day=="2016-01-19"|day=="2016-01-20"|day=="2016-01-21")),])
  
  #system.time(datalist$tr[[i]]<-alldata[which(!(day=="2016-01-15"|day=="2016-01-16" |day=="2016-01-17"|day=="2016-01-18"|day=="2016-01-19"|day=="2016-01-20"|day=="2016-01-21"|day=="2016-01-22"|day=="2016-01-24"|day=="2016-01-26"|day=="2016-01-28"|day=="2016-01-30")),])
  #system.time(datalist$te[[i]]<-alldata[which(!(day=="2016-01-02"|day=="2016-01-03"|day=="2016-01-04"|day=="2016-01-05"|day=="2016-01-06"|day=="2016-01-07"|day=="2016-01-08"|day=="2016-01-09"|day=="2016-01-10"|day=="2016-01-11"|day=="2016-01-12"|day=="2016-01-13"|day=="2016-01-14")),])
  
  
  trainingdata<-as.data.frame(datalist$tr[[i]])
  trainingdata<-rbind(trainingdata,trainingdata)
  testingdata<-as.data.frame(datalist$te[[i]])
  
  tr_lable<-trainingdata$y
  te_lable<-testingdata$y
  
  tr_flag<-trainingdata[,1:4]
  trdata<-trainingdata[,-c(1,2,3,4)]
  te_flag<-testingdata[,1:4]
  tedata<-testingdata[,-c(1,2,3,4)]
  
  marsGrid <- expand.grid(.degree = 1:5, .nprune = 2:38)
  set.seed(200)
  marsTuned[[i]] <- train(trainingdata[,c("a1","b1","c1","b4")], tr_lable,method = "earth",tuneGrid = marsGrid,trControl = trainControl(method = "cv"))
  
  
  pr<-predict(marsTuned[[i]], testingdata[,c("a1","b1","c1","b4")])
  
  lfile<-cbind(tebak,pr)
  
  ##########################################
  #wpr<-predict(marsTuned[[i]], prdata[,c("a1","b1","c1","b4")])
  #预估成负数的变为0    负数很少
  #wpr[wpr<0]<-0
  #预估的时间段是训练的时间段加1  为了提交的时候匹配
  #wfile<-cbind(prdata[,"map_block"],paste(prdata[,"day"],prdata[,"model_choose"]+1,sep="-"),wpr)
  #lfile<-cbind(prdatabak,prdata[,"map_block"],paste(prdata[,"day"],prdata[,"model_choose"],sep="-"),wpr)
  ###tail(cbind(prdata[,"map_block"],paste(prdata[,"day"],prdata[,"model_choose"]+1,sep="-"),wpr),20)
  
  #write.csv(wfile, file =paste("/Users/baidu/work/didi/updata/20160525/updata_",i,".csv",sep = ""), quote=FALSE, row.names=FALSE)
  #############################################
  ##测试的时候
  write.csv(lfile, file =paste("/Users/baidu/work/didi/updata/20160525/look_updata_",i,".csv",sep = ""), quote=FALSE, row.names=FALSE)
  
  #tmpdata<-cbind(te_flag[,1:3],pr,testingdata)
  #cbind(te_lable,pr)
  te_lable_new<-te_lable[te_lable>0]
  pr<-pr[te_lable>0]
  finalupdata<-sum(abs(te_lable_new-pr)/te_lable_new)/length(te_lable_new)
  cat("+++++++++++++++++++++++++  :i:",i,"  finalupdata:",finalupdata,"\n")    
}



predictfunc<- function(data,datalist){
  
  #select model data
  marsTuned<-list()
  #for(i in c(46,58,70,82,94,106,118,130,142)){
  for(i in c("45","57","69","81","93","105","117","129","141")){ 
    #    for(i in c("45","57","81","93","105","117","129","141")){ 
    
    system.time(datalist$all[[i]]<-data[which(!(model_choose!=i)),])
    
    
    alldata<-as.data.frame(datalist$all[[i]])
    
    trans <- preProcess(alldata[,-c(1,2,3,4)],method = c("pca"))
    transformed <- predict(trans, alldata[,-c(1,2,3,4)])
    alldata<-cbind(alldata[,c(1,2,3,4)],transformed[,1:3])
    
    testbak<-alldata[which(!(day=="2016-01-02"|day=="2016-01-03"|day=="2016-01-04"|day=="2016-01-05"|day=="2016-01-06"|day=="2016-01-07"|day=="2016-01-08"|day=="2016-01-09"|day=="2016-01-10"|day=="2016-01-11"|day=="2016-01-12"|day=="2016-01-13"|day=="2016-01-14"|day=="2016-01-15"|day=="2016-01-16" |day=="2016-01-17"|day=="2016-01-18"|day=="2016-01-19"|day=="2016-01-20"|day=="2016-01-21")),]
    
    
    
    fild_bak<-alldata[,1:4]
    alldata<-alldata[,5:16]
    #    for(j in c("a1","a2","a3","a4","b1","b2","b3","b4","c1","c2","c3","c4")){
    #      inf<-preNumDatafucYestodayImp(alldata[,j],getpr=1)
    #      alldata[,j]<-preNumDatafucYestodayImp(alldata[,j],inf=inf,getpr=0)
    #    }
    
    
    #trainingdata<-datadummyVars(trainingdata)
    #testingdata<-datadummyVars(testingdata)
    
    #alldata<-subsetVars(alldata)
    alldata<-alldata[,c(1,2,3,11)]
    set.seed(100)
    alldata<-cbind(fild_bak,alldata)
    #alldata<-datadummyVars(alldata)
    
    
    alldata<-as.data.table(alldata)
    
    system.time(datalist$tr[[i]]<-alldata[which(!(day=="2016-01-22"|day=="2016-01-24"|day=="2016-01-26"|day=="2016-01-28"|day=="2016-01-30")),])
    system.time(datalist$te[[i]]<-alldata[which(!(day=="2016-01-02"|day=="2016-01-03"|day=="2016-01-04"|day=="2016-01-05"|day=="2016-01-06"|day=="2016-01-07"|day=="2016-01-08"|day=="2016-01-09"|day=="2016-01-10"|day=="2016-01-11"|day=="2016-01-12"|day=="2016-01-13"|day=="2016-01-14"|day=="2016-01-15"|day=="2016-01-16" |day=="2016-01-17"|day=="2016-01-18"|day=="2016-01-19"|day=="2016-01-20"|day=="2016-01-21")),])
    
    #system.time(datalist$tr[[i]]<-alldata[which(!(day=="2016-01-15"|day=="2016-01-16" |day=="2016-01-17"|day=="2016-01-18"|day=="2016-01-19"|day=="2016-01-20"|day=="2016-01-21"|day=="2016-01-22"|day=="2016-01-24"|day=="2016-01-26"|day=="2016-01-28"|day=="2016-01-30")),])
    #system.time(datalist$te[[i]]<-alldata[which(!(day=="2016-01-02"|day=="2016-01-03"|day=="2016-01-04"|day=="2016-01-05"|day=="2016-01-06"|day=="2016-01-07"|day=="2016-01-08"|day=="2016-01-09"|day=="2016-01-10"|day=="2016-01-11"|day=="2016-01-12"|day=="2016-01-13"|day=="2016-01-14")),])
    
    
    trainingdata<-as.data.frame(datalist$tr[[i]])
    testingdata<-as.data.frame(datalist$te[[i]])
    
    
    tr_lable<-trainingdata$y
    te_lable<-testingdata$y
    
    tr_flag<-trainingdata[,1:4]
    trainingdata<-trainingdata[,-c(1,2,3,4)]
    te_flag<-testingdata[,1:4]
    testingdata<-testingdata[,-c(1,2,3,4)]
    
    marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
    
    marsTuned[[i]] <- train(trainingdata, tr_lable,method = "earth",
                            tuneGrid = marsGrid,trControl = trainControl(method = "cv"))
    #marsTuned[[i]] <- earth(trainingdata, tr_lable)
    #paste(cbind(cbind(c("aa","bb")),cbind(c("cc","dd"))))
    
    wpr<-predict(marsTuned[[i]], testingdata)
    
    
    wpr[wpr<0]<-0
    
    #head(cbind(te_lable,wpr))
    te_lable_new<-te_lable[te_lable>0]
    wpr<-wpr[te_lable>0]
    
    finalerr<-sum(abs(te_lable_new-wpr)/te_lable_new)/(66*5)
    cat("++++++++++:i:<",i,"> degree   final loss: ",finalerr,"\n") 
    
    
    
    
    
    
    #  wpr<-predict(marsTuned[[i]], prdata[,c(5,6,7,15)])
    
    #paste(prdata[,"day"],prdata[,"model_choose"],sep="-")
    lfile<-cbind(testbak,datalist$te[[i]]$map_block,paste(datalist$te[[i]]$day,datalist$te[[i]]$model_choose,sep="-"),pr)
    #    lfile<-cbind(prdata1,prdata[,"map_block"],paste(prdata[,"day"],prdata[,"model_choose"],sep="-"),wpr)
    
    #    write.csv(wfile, file =paste("/Users/baidu/work/didi/updata/20160525/updata_",i,".csv",sep = ""), quote=FALSE, row.names=FALSE)
    write.csv(lfile, file =paste("/Users/baidu/work/didi/updata/20160525/look_updata_",i,".csv",sep = ""), quote=FALSE, row.names=FALSE)
    
    #tmpdata<-cbind(te_flag[,1:3],pr,testingdata)
    #cbind(te_lable,pr)
    te_lable_new<-te_lable[te_lable>0]
    pr<-pr[te_lable>0]
    finalupdata<-sum(abs(te_lable_new-pr)/te_lable_new)/length(te_lable_new)
    cat("+++++++++++++++++++++++++  :i:",i,"  finalupdata:",finalupdata,"\n")    
    
  }
  return (marsTuned)
}


main<-function(data,dayv,flag){
  system.time(data<-fread("/Users/baidu/work/didi/trainingdata20160122.csv", sep = ","))
  system.time(data2<-fread("/Users/baidu/work/didi/testingdata20160523.csv", sep = ","))
  system.time(dayv<-fread("/Users/baidu/work/didi/day.csv", sep = ","))
  #system.time(dayv<-fread("/Users/baidu/work/didi/dayweek.csv", sep = ","))
  
  #去掉元旦数据
  system.time(data<-data[which(!(day=="2016-01-01")),])
  system.time(data2<-data2[which(!(day=="2016-01-01")),])
  data2<-data2[which(!(day=="2016-01-22"|day=="2016-01-24"|day=="2016-01-26"|day=="2016-01-28"|day=="2016-01-30")),]
  
  prtestingdata<-data2[which(!(day=="2016-01-02"|day=="2016-01-03"|day=="2016-01-04"|day=="2016-01-05"|day=="2016-01-06"|day=="2016-01-07"|day=="2016-01-08"|day=="2016-01-09"|day=="2016-01-10"|day=="2016-01-11"|day=="2016-01-12"|day=="2016-01-13"|day=="2016-01-14"|day=="2016-01-15"|day=="2016-01-16" |day=="2016-01-17"|day=="2016-01-18"|day=="2016-01-19"|day=="2016-01-20"|day=="2016-01-21")),]
  
  
  
  data<-rbind(data,data2)
  
  
  
  #system.time(data2<-data2[which(!(model_choose=="45")),])
  #data2$model_choose[data2$model_choose=="45_1"]<-"45"
  
  
  # data_bak<-as.data.frame(data)
  #  data_bak<-data_bak[,1:3]
  
  #  data<-pro_day(data,dayv,1)
  # data<-as.data.frame(data)
  #  data<-cbind(data_bak,data[4:16])
  
  datalist<-list()
  datalist$data<-list()
  datalist$te<-list()
  datalist$tr<-list()
  
  
  # tmpdata_bak<-tmpdata[,1:3]
  
  #tmpdata<-pro_day(tmpdata,dayv,2)
  #  tmpdata<-as.data.frame(tmpdata)
  #  tmpdata<-cbind(tmpdata_bak,tmpdata[4:9])
  
  
  marsTuned<-predictfunc(data,datalist)
  
  
  #  pr[[1]]<-predict(marsTuned[length(marsTuned)], testingdata)
  #  pr<-as.data.frame(pr[[1]])
  #  head(cbind(pr,te_lable,testingdata[,1:3]))
  
  
  #tmpdata<-cbind(te_flag[,1:3],pr,testingdata)
  #cbind(te_lable,pr)
  #  te_lable_new<-te_lable[te_lable>0]
  #  pr<-as.numeric(pr[[1]])
  #  pr<-pr[te_lable>0]
  #  finalupdata<-sum(abs(te_lable_new-pr)/te_lable_new)/length(te_lable_new)
  #  cat("+++++++++++++++++++++++++  :i:",i,"  finalupdata:",finalupdata,"\n")    
  
  
}



#main()

########################################################最终提交的测试集只有一个
prtestingdata<-data2[which(!(day=="2016-01-02"|day=="2016-01-03"|day=="2016-01-04"|day=="2016-01-05"|day=="2016-01-06"|day=="2016-01-07"|day=="2016-01-08"|day=="2016-01-09"|day=="2016-01-10"|day=="2016-01-11"|day=="2016-01-12"|day=="2016-01-13"|day=="2016-01-14"|day=="2016-01-15"|day=="2016-01-16" |day=="2016-01-17"|day=="2016-01-18"|day=="2016-01-19"|day=="2016-01-20"|day=="2016-01-21")),]

#########################################################预估  45
i=45
system.time(data<-fread("/Users/baidu/work/didi/trainingdata20160122.csv", sep = ","))
system.time(data2<-fread("/Users/baidu/work/didi/testingdata20160523.csv", sep = ","))
#去掉元旦数据
system.time(data<-data[which(!(day=="2016-01-01")),])
system.time(data2<-data2[which(!(day=="2016-01-01")),])
data<-rbind(data,data2)
#data<-data[which(!(map_block=="8"|map_block=="14"|map_block=="23"|map_block=="37"|map_block=="46"|map_block=="51")),]

buildtestdata(data,prtestingdata,45)

------------------------------------------------------------57
i=57
system.time(data2<-fread("/Users/baidu/work/didi/testingdata20160523.csv", sep = ","))
#去掉元旦数据
system.time(data2<-data2[which(!(day=="2016-01-01")),])
data<-data2
buildtestdata(data,prtestingdata,57)

#########################################################预估  69
i=69
system.time(data<-fread("/Users/baidu/work/didi/trainingdata20160122.csv", sep = ","))
system.time(data2<-fread("/Users/baidu/work/didi/testingdata20160523.csv", sep = ","))
#去掉元旦数据

data<-rbind(data,data2)
buildtestdata(data,prtestingdata,69)


#########################################################预估  81
i=81
system.time(data<-fread("/Users/baidu/work/didi/trainingdata20160122.csv", sep = ","))
system.time(data2<-fread("/Users/baidu/work/didi/testingdata20160523.csv", sep = ","))
#去掉元旦数据

data<-rbind(data,data2)
buildtestdata(data,prtestingdata,81)


------------------------------------------------------------93
i=93
system.time(data2<-fread("/Users/baidu/work/didi/testingdata20160523.csv", sep = ","))
#去掉元旦数据
data<-data2
buildtestdata(data,prtestingdata,93)

------------------------------------------------------------105
i=105
system.time(data2<-fread("/Users/baidu/work/didi/testingdata20160523.csv", sep = ","))
#去掉元旦数据
system.time(data2<-data2[which(!(day=="2016-01-01")),])
data<-data2
buildtestdata(data,prtestingdata,105)
------------------------------------------------------------117
i=117
system.time(data2<-fread("/Users/baidu/work/didi/testingdata20160523.csv", sep = ","))
#去掉元旦数据
system.time(data2<-data2[which(!(day=="2016-01-01")),])
data<-data2
buildtestdata(data,prtestingdata,117)

#########################################################预估  129
i=129
system.time(data<-fread("/Users/baidu/work/didi/trainingdata20160122.csv", sep = ","))
system.time(data2<-fread("/Users/baidu/work/didi/testingdata20160523.csv", sep = ","))
#去掉元旦数据

data<-rbind(data,data2)
buildtestdata(data,prtestingdata,129)


------------------------------------------------------------141
i=141
system.time(data2<-fread("/Users/baidu/work/didi/testingdata20160523.csv", sep = ","))
#去掉元旦数据
system.time(data2<-data2[which(!(day=="2016-01-01")),])
data<-data2
buildtestdata(data,prtestingdata,141)

