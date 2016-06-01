#在电脑上把训练集和测试集合成一个大文件 然后执行下面的 shell 在后面加上时间分片
#cat alldata |awk -F"\t" '{str_time=$7;split(str_time,A," "); split(A[2],B,":") ; time_m=B[1]*60+B[2];idx=int(time_m/10); if(time_m%10!=0){idx=idx+1};$(NF+1)=idx;print $0 }' > testingdata_add_tid

########################    改函数要每次运行下这个加载一下.
###      <执行前改下 data 路径和 bin 路径>

#######  didi_lib.R 加载包,
#        func.R 把特征筛选和数值型特征归一化函数初始化一下
#        didi_init_data.R 把原始数据读进来整理成模型需要的格式
#        didi_lib.R   9个小模型  开始训练

#######  
#source("/Users/baidu/work/didi/bin/didi_config.R")
#source(paste(BINDIR,"/didi_lib.R",sep = ""))
#source(paste(BINDIR,"/didi_func.R",sep = ""))
#source(paste(BINDIR,"/didi_init_data.R",sep = ""))

#######   
#######    最好单步执行,一条一条的跑,看下数据每步处理成啥样了 
#         执行每一行数据 mac 是 command +return    windows 好像是 control+R   
#           常用的看数据函数  head()  tail()  summary() 
#           查看数据的类型  用 class()  
#           给数据转格式 as.data.frame()   as.** 
#           相同行或者列   拷贝到一起  用 cbind()  rbind()
#           查看函数,或者模型返回值,或者数据,用 str()
#           如果那个函数不知道意思  help(函数名)  注解很详细
#           如果想看哪个包里都有哪些函数  用两个问好加包名   ??packagenames
#           要用 r 的 ide  敲代码的时候数据数据补全以及编辑都很好用

#           常用的两个数据格式是 data.frame 和 data.table,这两个数据格式在筛选数据的时候稍有不一样
#           

############################ 读取数据 然后把数据汇总


#######  i 是预估的时间段  == 46    58   70   82   94   106    118     130    142
######
########### head()  或者 summary()  一下数据就知道数据是啥样了
#########        trdata_y 是先取到训练集 用要提交的时间段数据的 day,tid,mid,汇总结果,
#########        然后去关联它前几个时间片的汇总结果,当然测试集是拿不到提交时间片的结果的.
#########        所以 trdata_y 只针对1到21号的测试数据.
#########        tedata_y  这个是根据要提交的前一个时间段的汇总结构关联它前几个时间片的数据
#########        后面我会对 tedata 做筛选,值要22号到30号的汇总,用汇总去预估下一个时间片,
#########        也就是要提交时间片.

###########离线验证效果

offlineflag<-1   #####要提交的时候设置成0
onlineflag<-0    ##### 提交时设置成1
#system.time(dayw<-fread("/Users/baidu/work/didi/data/dayweek.csv", sep = ","))
cutfild<-6
marsTuned<<-list()
trbak<<-list()
tebak<<-list()

############ 训练集和测试集选取的维度
select_filed<-c("yXday_mid","yXday","yXtimeid","yXstart_mid","yXv1","yXv2","yXdriver_id_null",
                "x1X1","x1X2","x1X3","x1X4","x1X5","x1X6","x1X7","x1X8","x1X9","x1X10","x1X11","x1X12","x1X13","x1X14","x1X15","x1X16","x1X17","x1X18","x1X19","x1X20","x1X21","x1X22","x1X23","x1X24","x1X25","x1X26","x1X27","x1X28","x1X29","x1X30","x1X31","x1X32","x1X33","x1X34","x1X35","x1X36","x1X37","x1X38","x1X39","x1X40","x1X41","x1X42","x1X43","x1X44","x1X45","x1X46","x1X47","x1X48","x1X49","x1X50","x1X51","x1X52","x1X53","x1X54","x1X55","x1X56","x1X57","x1X58","x1X59","x1X60","x1X61","x1X62","x1X63","x1X64","x1X65","x1X66","x1X100",
                "x1Xcount_ordernum","x1Xav_price",
                "x2X1","x2X2","x2X3","x2X4","x2X5","x2X6","x2X7","x2X8","x2X9","x2X10","x2X11","x2X12","x2X13","x2X14","x2X15","x2X16","x2X17","x2X18","x2X19","x2X20","x2X21","x2X22","x2X23","x2X24","x2X25","x2X26","x2X27","x2X28","x2X29","x2X30","x2X31","x2X32","x2X33","x2X34","x2X35","x2X36","x2X37","x2X38","x2X39","x2X40","x2X41","x2X42","x2X43","x2X44","x2X45","x2X46","x2X47","x2X48","x2X49","x2X50","x2X51","x2X52","x2X53","x2X54","x2X55","x2X56","x2X57","x2X58","x2X59","x2X60","x2X61","x2X62","x2X63","x2X64","x2X65","x2X66","x2X100",
                "x2Xcount_ordernum","x2Xav_price",
                "x3X1","x3X2","x3X3","x3X4","x3X5","x3X6","x3X7","x3X8","x3X9","x3X10","x3X11","x3X12","x3X13","x3X14","x3X15","x3X16","x3X17","x3X18","x3X19","x3X20","x3X21","x3X22","x3X23","x3X24","x3X25","x3X26","x3X27","x3X28","x3X29","x3X30","x3X31","x3X32","x3X33","x3X34","x3X35","x3X36","x3X37","x3X38","x3X39","x3X40","x3X41","x3X42","x3X43","x3X44","x3X45","x3X46","x3X47","x3X48","x3X49","x3X50","x3X51","x3X52","x3X53","x3X54","x3X55","x3X56","x3X57","x3X58","x3X59","x3X60","x3X61","x3X62","x3X63","x3X64","x3X65","x3X66","x3X100",
                "x3Xcount_ordernum","x3Xav_price")
########  要处理的数值型特征
numeric_fild<-c("x1X1","x1X2","x1X3","x1X4","x1X5","x1X6","x1X7","x1X8","x1X9","x1X10","x1X11","x1X12","x1X13","x1X14","x1X15","x1X16","x1X17","x1X18","x1X19","x1X20","x1X21","x1X22","x1X23","x1X24","x1X25","x1X26","x1X27","x1X28","x1X29","x1X30","x1X31","x1X32","x1X33","x1X34","x1X35","x1X36","x1X37","x1X38","x1X39","x1X40","x1X41","x1X42","x1X43","x1X44","x1X45","x1X46","x1X47","x1X48","x1X49","x1X50","x1X51","x1X52","x1X53","x1X54","x1X55","x1X56","x1X57","x1X58","x1X59","x1X60","x1X61","x1X62","x1X63","x1X64","x1X65","x1X66","x1X100",
                "x1Xdriver_id_null","x1Xcount_ordernum","x1Xav_price",
                "x2X1","x2X2","x2X3","x2X4","x2X5","x2X6","x2X7","x2X8","x2X9","x2X10","x2X11","x2X12","x2X13","x2X14","x2X15","x2X16","x2X17","x2X18","x2X19","x2X20","x2X21","x2X22","x2X23","x2X24","x2X25","x2X26","x2X27","x2X28","x2X29","x2X30","x2X31","x2X32","x2X33","x2X34","x2X35","x2X36","x2X37","x2X38","x2X39","x2X40","x2X41","x2X42","x2X43","x2X44","x2X45","x2X46","x2X47","x2X48","x2X49","x2X50","x2X51","x2X52","x2X53","x2X54","x2X55","x2X56","x2X57","x2X58","x2X59","x2X60","x2X61","x2X62","x2X63","x2X64","x2X65","x2X66","x2X100",
                "x2Xdriver_id_null","x2Xcount_ordernum","x2Xav_price",
                "x3X1","x3X2","x3X3","x3X4","x3X5","x3X6","x3X7","x3X8","x3X9","x3X10","x3X11","x3X12","x3X13","x3X14","x3X15","x3X16","x3X17","x3X18","x3X19","x3X20","x3X21","x3X22","x3X23","x3X24","x3X25","x3X26","x3X27","x3X28","x3X29","x3X30","x3X31","x3X32","x3X33","x3X34","x3X35","x3X36","x3X37","x3X38","x3X39","x3X40","x3X41","x3X42","x3X43","x3X44","x3X45","x3X46","x3X47","x3X48","x3X49","x3X50","x3X51","x3X52","x3X53","x3X54","x3X55","x3X56","x3X57","x3X58","x3X59","x3X60","x3X61","x3X62","x3X63","x3X64","x3X65","x3X66","x3X100",
                "x3Xdriver_id_null","x3Xcount_ordernum","x3Xav_price")

#write.csv(tmp, file =paste(OUTPUT_DIR,"/tmp.csv",sep = ""), quote=FALSE, row.names=FALSE)

####################### 全局变量  for 循环执行完也可以访问到

for(i in c(46,58,70,82,94,106,118,130,142)){     
  i<-(i-offlineflag)  
  #  cat("################ 提取预估时间片:",i,",的训练数据\n")
  data<-as.data.table(data)
  trdata_y<-data[which((timeid==i)),]
  trdata_x1<-data[which((timeid==(i-1))),]
  trdata_x2<-data[which((timeid==(i-2))),]
  trdata_x3<-data[which((timeid==(i-3))),]
  trdata_y<-as.data.table(trdata_y)
  trdata_x1<-as.data.table(trdata_x1)
  trdata_x2<-as.data.table(trdata_x2)
  trdata_x3<-as.data.table(trdata_x3)
  dim(trdata_y)
  trdata_y$day_mid=paste(trdata_y$day,trdata_y$start_mid,sep = ":")
  trdata_x1$day_mid=paste(trdata_x1$day,trdata_x1$start_mid,sep = ":")
  trdata_x2$day_mid=paste(trdata_x2$day,trdata_x2$start_mid,sep = ":")
  trdata_x3$day_mid=paste(trdata_x3$day,trdata_x3$start_mid,sep = ":")
  
  name_y<-paste("y",names(trdata_y),sep = "X")
  names(trdata_y)<-name_y
  
  name_x1<-paste("x1",names(trdata_x1),sep = "X")
  names(trdata_x1)<-name_x1
  
  name_x2<-paste("x2",names(trdata_x2),sep = "X")
  names(trdata_x2)<-name_x2
  
  
  name_x3<-paste("x3",names(trdata_x3),sep = "X")
  names(trdata_x3)<-name_x3
  #  cat("################ 数据排序,为后面的 join 优化时间\n")
  trdata_y<-trdata_y[order(trdata_y[,yXday_mid],decreasing=TRUE),] 
  trdata_x1<-trdata_x1[order(trdata_x1[,x1Xday_mid],decreasing=TRUE),] 
  trdata_x2<-trdata_x2[order(trdata_x2[,x2Xday_mid],decreasing=TRUE),] 
  trdata_x3<-trdata_x3[order(trdata_x3[,x3Xday_mid],decreasing=TRUE),]
  dim(trdata_y)
  
  #  cat("################  把前几个时间片的数据 join 过来\n")
  #tr_day_mid<-get_tr_day_mid()
  #tr_day_mid<-as.data.frame(tr_day_mid)
  #  write.csv(trainingdata, file =paste(OUTPUT_DIR,"/look.csv",sep = ""), quote=FALSE, row.names=FALSE)
  
  trainingdata<-merge(trdata_y,trdata_x1,by.x="yXday_mid",by.y ="x1Xday_mid",all.x=TRUE)
  trainingdata<-merge(trainingdata,trdata_x2,by.x="yXday_mid",by.y = "x2Xday_mid",all.x = TRUE)
  trainingdata<-merge(trainingdata,trdata_x3,by.x="yXday_mid",by.y = "x3Xday_mid",all.x = TRUE)
  trainingdata<-subset(trainingdata,select=select_filed)
  trainingdata<-as.data.table(trainingdata)
  dim(trainingdata)
  
  if(onlineflag==1)
  {
    #    cat("################ 提取预估时间片:",i,",的测试数据\n")
    ############  
    tedata_y<-data[which((timeid==(i-onlineflag))),]
    
    tedata_x1<-data[which((timeid==(i-1))),]
    tedata_x2<-data[which((timeid==(i-2))),]
    tedata_x3<-data[which((timeid==(i-3))),]
    tedata_y<-as.data.table(tedata_y)
    tedata_x1<-as.data.table(tedata_x1)
    tedata_x2<-as.data.table(tedata_x2)
    tedata_x3<-as.data.table(tedata_x3)
    
    tedata_y$day_mid<-paste(tedata_y$day,tedata_y$start_mid,sep = ":")
    tedata_x1$day_mid<-paste(tedata_x1$day,tedata_x1$start_mid,sep = ":")
    tedata_x2$day_mid<-paste(tedata_x2$day,tedata_x2$start_mid,sep = ":")
    tedata_x3$day_mid<-paste(tedata_x3$day,tedata_x3$start_mid,sep = ":")
    
    names(tedata_y)<-name_y
    names(tedata_x1)<-name_x1
    names(tedata_x2)<-name_x2
    names(tedata_x3)<-name_x3
    
    
    tedata_y<-tedata_y[order(tedata_y[,yXday_mid],decreasing=TRUE),] 
    tedata_x1<-tedata_x1[order(tedata_x1[,x1Xday_mid],decreasing=TRUE),] 
    tedata_x2<-tedata_x2[order(tedata_x2[,x2Xday_mid],decreasing=TRUE),] 
    tedata_x3<-tedata_x3[order(tedata_x3[,x3Xday_mid],decreasing=TRUE),] 
    
    #   te_day_mid<-get_te_day_mid()
    testingdata<-merge(tedata_y,tedata_x1,by.x="yXday_mid",by.y = "x1Xday_mid",all.x= TRUE)
    testingdata<-merge(testingdata,tedata_x2,by.x="yXday_mid",by.y = "x2Xday_mid",all.x= TRUE)
    testingdata<-merge(testingdata,tedata_x3,by.x="yXday_mid",by.y = "x3Xday_mid",all.x= TRUE)
    testingdata<-subset(testingdata,select=select_filed)
    testingdata<-as.data.table(testingdata)
    dim(testingdata)
  }
  
  dim(trainingdata)
  ########### 测试集是22号30号的
  if(onlineflag==1)
  {
    testingdata<-testingdata[which((yXday=="2016-01-22"|yXday=="2016-01-24"|yXday=="2016-01-26"|yXday=="2016-01-28"|yXday=="2016-01-30")),]
    alldata<-rbind(trainingdata,testingdata)
    #testingdata$driver_id_nullx<-0
  }else{
    alldata<-trainingdata
  }
  #################   每时间片做 join 的时候有的没关联上 10% 没关联上
  #sum(is.na((alldata)))/(dim(alldata)[1]*dim(alldata)[2])
  alldata[is.na((alldata))]<-(-1)
  # alldata[alldata==-1]<-(0)    loss 0.3103715 
  
  alldata<-as.data.frame(alldata)
  #dim(alldata)
  
  ############ 这块合一起   是为了在归一化的时候 也考虑到22到30号的数据分布
  ########### 随机森林不用归一化
  #  for(j in numeric_fild){
  #    inf=preNumDatafucYestodayImp(alldata[,j],getpr=1)
  #    alldata[,j]=preNumDatafucYestodayImp(alldata[,j],inf=inf,getpr=0)
  #  }
  
  ###############  特征筛选
  alldata$yXtimeid<-NULL
  alldata<-subsetVars(alldata)  #### 把一行都是一项的都删掉了,包括 tid...
  alldata<-as.data.table(alldata)
  
  alldata<-merge(alldata,dayw,by.x="yXday",by.y = "day",all.x= TRUE)
  #
  alldata<-as.data.frame(alldata)
  
  
  alldata_a<-datadummyVarsbuy(alldata[,-c(1:6)])
  alldata<-cbind(alldata[,c(1:6)],alldata_a)
  alldata<-as.data.table(alldata)
  alldata$yXv<-alldata$yXv2
  #alldata$yXv2<-NULL
  #alldata<-merge(alldata,poi_map,by.x="yXstart_mid",by.y="district_id",all.x = TRUE)
  
  alldata$dayXtid<-cbind(paste(alldata$yXday,i,sep = "-"))
  alldata<-merge(alldata,weather,by.x="dayXtid",by.y="day_tid",all.x = TRUE)
  alldata$dayXtid<-NULL
  
  #########################     <  PCA   >     ########################## pca
  alldata<-as.data.frame(alldata)
  alldata[is.na((alldata))]<-(-1)
  
  #  trans <- preProcess(alldata[,-c(1:cutfild)],method = c("pca"))
  #  transformed <- predict(trans, alldata[,-c(1:cutfild)])
  #  alldata<-cbind(alldata[,c(1:cutfild)],transformed[,1:2])
  #################
  
  alldata<-as.data.table(alldata)
  
  trainingdata<-alldata[which(!(yXday=="2016-01-22"|yXday=="2016-01-24"|yXday=="2016-01-26"|yXday=="2016-01-28"|yXday=="2016-01-30")),]
  if((i==45)|(i==46))
  {
    testingdata<-alldata[which((yXday=="2016-01-22"|yXday=="2016-01-26"|yXday=="2016-01-30")),]
  }else{
    testingdata<-alldata[which((yXday=="2016-01-22"|yXday=="2016-01-24"|yXday=="2016-01-26"|yXday=="2016-01-28"|yXday=="2016-01-30")),]
  }
  
  dim(trainingdata)
  dim(testingdata)
  
  trainingdata<-as.data.frame(trainingdata)
  testingdata<-as.data.frame(testingdata)
  trainingdata[is.na((trainingdata))]<-(-1)
  testingdata[is.na((testingdata))]<-(-1)
  
  
  names(trainingdata)[grepl("yXdriver_id_null", names(trainingdata))]<-"y"
  
  tr_lable<-trainingdata$y
  #tr_lable<-tr_lable
  # tr_lable[tr_lable==0]<-(-1)
  size<-dim(trainingdata)[2]
  trdata<-trainingdata[,-c(1:cutfild)]
  
  names(testingdata)[grepl("yXdriver_id_null", names(testingdata))]<-"y"
  trbak[[i]]<-trainingdata
  tebak[[i]]<-testingdata
  
  te_lable<-testingdata$y
  
  tedata<-testingdata[,-c(1:cutfild)]
  
  dim(testingdata)
  
  ######################################################################################
  
  set.seed(100)
  ######################### 线性回归
  # marsTuned[[i]] <- lm(y ~ ., data = trainingdata[,(cutfild):size])
  #  wpr<-predict(marsTuned[[i]], tedata)
  ######################### MARS 算法
  #  if((i==45|i==46))
  #  {
  #    n1<-1;n2<-3;       m1<-2;m2<-8
  #  }else if((i==105|i==106|i==141|i==142))
  #  {
  #    n1<-1;n2<-3;       m1<-2;m2<-22
  #  }else{
  #    n1<-1;n2<-2;       m1<-2;m2<-10
  #  }
  #  n1<-1
  #  n2<-3
  #  m1<-2
  #  m2<-38
  #  degree<-n1:n2
  #  nprune<-m1:m2
  #  ctrl <- trainControl(method = "cv")
  #  marsGrid <- expand.grid(.degree = degree, .nprune = nprune)
  #  marsTuned[[i]] <- train(trdata, tr_lable,method = "earth",
  #                         tuneGrid = marsGrid,trControl = ctrl)
  #   marsTuned[[i]] <- earth(trdata, tr_lable)
  ########################  随机森林
  #  marsTuned[[i]] <- randomForest(trdata, tr_lable,
  #                          importance = TRUE,
  #                          ntrees = 300)
  ############################  随机森林
  #marsTuned[[i]]  <- randomForest(trdata, tr_lable)
  ##############################  ....树
  #marsTuned[[i]] <- ipredbagg(trdata, tr_lable)
  ############################################
  marsTuned[[i]] <- train(trdata, tr_lable,
                          method = "rpart2",
                          tuneLength = 10,
                          trControl = trainControl(method = "cv"))
  
  ################################  MARS 简洁版
  #    marsTuned[[i]] <- enet(x = as.matrix(trdata), y = tr_lable,
  #                       lambda = 0.001)
  #    pr <- predict(marsTuned[[i]], newx = as.matrix(tedata),
  #                   s = 1, mode = "fraction", type = "fit")
  #    wpr<-pr$fit
  ###############################   bossted trees
  #gbmGrid <- expand.grid(.interaction.depth = seq(1, 7, by = 2), 
  #                      .n.trees = seq(100, 1000, by = 50),
  #                       .shrinkage = c(0.01, 0.1))
  #data(solubility)
  #marsTuned[[i]] <- train(solTrainXtrans, solTrainY,
  #                        method = "gbm",
  #                tuneGrid = gbmGrid,
  #                verbose = FALSE)
  ###################  gxboost
  
  #marsTuned[[i]] <- gbm.fit(trdata, tr_lable, distribution = "gaussian") 
  ## or
  # marsTuned[[i]] <- lm(y ~ ., data = trainingdata[,(cutfild):size])
  #  wpr<-predict(marsTuned[[i]], tedata)
  #marsTuned[[i]]<- gbm(y ~ ., data = trainingdata[,(cutfild):size], distribution = "gaussian")
  
  ##############
  #f <- ~ x1X1+x1X2+x1X3+x1X4+x1X5+x1X6+x1X7+x1X8+x1X9+x1X10+x1X11+x1X12+x1X13+x1X14+x1X15+x1X16+x1X17+x1X18+x1X19+x1X20+x1X21+x1X22+x1X23+x1X24+x1X25+x1X26+x1X27+x1X28+x1X29+x1X30+x1X31+x1X32+x1X33+x1X34+x1X35+x1X36+x1X37+x1X38+x1X39+x1X40+x1X41+x1X42+x1X43+x1X44+x1X45+x1X46+x1X47+x1X48+x1X49+x1X50+x1X51+x1X52+x1X53+x1X54+x1X55+x1X56+x1X57+x1X58+x1X59+x1X60+x1X61+x1X62+x1X64+x1X65+x1X66+x1X100+x1Xcount_ordernum+x1Xav_price+x2X1+x2X2+x2X3+x2X4+x2X5+x2X6+x2X7+x2X8+x2X9+x2X10+x2X11+x2X12+x2X13+x2X14+x2X15+x2X16+x2X17+x2X18+x2X19+x2X20+x2X21+x2X22+x2X23+x2X24+x2X25+x2X26+x2X27+x2X28+x2X29+x2X30+x2X31+x2X32+x2X33+x2X34+x2X35+x2X36+x2X37+x2X38+x2X39+x2X40+x2X41+x2X42+x2X43+x2X44+x2X45+x2X46+x2X47+x2X48+x2X49+x2X50+x2X51+x2X52+x2X53+x2X54+x2X55+x2X56+x2X57+x2X58+x2X59+x2X60+x2X61+x2X62+x2X63+x2X64+x2X65+x2X66+x2X100+x2Xcount_ordernum+x2Xav_price+x3X1+x3X2+x3X3+x3X4+x3X5+x3X6+x3X7+x3X8+x3X9+x3X10+x3X11+x3X12+x3X13+x3X14+x3X15+x3X16+x3X17+x3X18+x3X19+x3X20+x3X21+x3X22+x3X23+x3X24+x3X25+x3X26+x3X27+x3X28+x3X29+x3X30+x3X31+x3X32+x3X33+x3X34+x3X35+x3X36+x3X37+x3X38+x3X39+x3X40+x3X41+x3X42+x3X43+x3X44+x3X45+x3X46+x3X47+x3X48+x3X49+x3X50+x3X51+x3X52+x3X53+x3X54+x3X55+x3X56+x3X57+x3X58+x3X59+x3X60+x3X61+x3X62+x3X64+x3X65+x3X66+x3X100+x3Xcount_ordernum+x3Xav_price+week1+week2+week3+week4+week5+week6+week7+weather+temper+PM25+split(mipid, delim = "\t")
  #  f<- ~ PC1+PC2+PC3
  #  trdata$x1Xav_price<-round(trdata$x1Xav_price)
  #  trdata$x2Xav_price<-round(trdata$x2Xav_price)
  #  trdata$x3Xav_price<-round(trdata$x3Xav_price)
  
  # trdata$PC1<-round(trdata$PC1,4)
  #  trdata$PC2<-round(trdata$PC2,4)
  #  trdata$PC3<-round(trdata$PC3,4)
  
  #  tedata$PC1<-round(tedata$PC1,4)
  #  tedata$PC2<-round(tedata$PC2,4)
  #  tedata$PC3<-round(tedata$PC3,4)
  
  
  #  m.train <- hashed.model.matrix(f, trdata, 2^20)
  #  m.test <- hashed.model.matrix(f, tedata, 2^20)
  
  #   dtrain <- xgb.DMatrix(m.train, label = tr_lable)
  #  dtest <- dtrain
  #dtest <- xgb.DMatrix(m.test, label = te_lable)
  #  watchlist <- list(eval = dtest, train = dtrain)
  #  param <- list(max.depth = 1300, eta = 0.5, silent = 0,lambda_bias=0.5, objective="reg:linear")
  #  marsTuned[[i]] <- xgb.train(param, dtrain, nthread = 2, nround = 2, watchlist)
  #  wpr<-predict(marsTuned[[i]],m.test)
  
  #  tmp<-names(trdata)
  #  write.csv(tmp, file =paste(OUTPUT_DIR,"/tmp.csv",sep = ""), quote=FALSE, row.names=FALSE)
  ######################################
  #marsTuned[[i]] <- cubist(trdata, tr_lable)  
  ########################################################
  
  
  #  marsTuned[[i]] <- nnet(trdata, tr_lable,size = 5,decay = 0.01,
  #                  linout = TRUE,
  #                  ## Reduce the amount of printed output
  #                  trace = FALSE,
  #                  ## Expand the number of iterations to find
  #                  ## parameter estimates..
  #                  maxit = 800,
  #                  ## and the number of parameters used by the model
  #                  MaxNWts = 5 * (ncol(trdata) + 1) + 5 + 1)
  
  #  marsTuned[[i]] <- avNNet(trdata, tr_lable,
  #                    size = 5,
  #                    decay = 0.01,
  #                    ## Specify how many models to average
  #                    repeats = 5,
  #                    linout = TRUE,
  #                    ## Reduce the amount of printed output
  #                    trace = FALSE,
  #                    ## Expand the number of iterations to find
  #                    ## parameter estimates..
  #                    maxit = 500,
  #                    ## and the number of parameters used by the model
  #                    MaxNWts = 5 * (ncol(trdata) + 1) + 5 + 1)
  ####################################################
  #ctrl <- trainControl(method = "cv")
  #nnetGrid <- expand.grid(.decay = c(0, 0.01, .1),.size = c(1:10),.bag = FALSE)
  ## The next option is to use bagging (see the
  ## next chapter) instead of different random
  ## seeds.
  
  #  marsTuned[[i]] <- train(trdata, tr_lable,
  #                   method = "avNNet",
  #                  tuneGrid = nnetGrid,trControl = ctrl,
  ## Automatically standardize data prior to modeling
  ## and prediction
  #                 preProc = c("center", "scale"),
  #                linout = TRUE,
  #               trace = FALSE,
  #              MaxNWts = 10 * (ncol(trainXnnet) + 1) + 10 + 1,
  #             maxit = 500)
  ###################################################
  
  wpr<-predict(marsTuned[[i]],tedata)
  wpr[wpr<0]<-0
  
  tid<-i
  mid<-testingdata$yXstart_mid
  day<-testingdata$yXday
  day_tid<-as.character(paste(day,tid,sep = "-"))
  wpr<-as.numeric(wpr)
  wfile<-as.data.frame(cbind(mid,day_tid,wpr))
  
  today <-paste(Sys.Date())
  if((offlineflag==1))
  {
    write.csv(trainingdata, file =paste(DATADIR,"/offline/trainingdata_",i,"_",today,".csv",sep = ""), quote=FALSE, row.names=FALSE)
    write.csv(testingdata, file =paste(DATADIR,"/offline/testingdata_",i,"_",today,".csv",sep = ""), quote=FALSE, row.names=FALSE)
    write.csv(wfile, file =paste(OUTPUT_DIR,"/offline_updata_",i,".csv",sep = ""), quote=FALSE, row.names=FALSE)
    
  }else{
    write.csv(trainingdata, file =paste(DATADIR,"/online/trainingdata_",i,"_",today,".csv",sep = ""), quote=FALSE, row.names=FALSE)
    testingdata$y<-0
    write.csv(testingdata, file =paste(DATADIR,"/online/testingdata_",i,"_",today,".csv",sep = ""), quote=FALSE, row.names=FALSE)
    write.csv(wfile, file =paste(OUTPUT_DIR,"/online_updata_",i,".csv",sep = ""), quote=FALSE, row.names=FALSE)
  }
  
  
  #  today <-Sys.Date()
  #  format(today, format="%B %d %Y")
  
  #  names(trainingdata)[grepl("yXdriver_id_null", names(trainingdata))]
  #  d_tid_mid<-strsplit(as.character(testingdata[,2]),":") 
  #  day<-sapply(d_tid_mid,"[",1) 
  #  tid<-sapply(d_tid_mid,"[",2) 
  #  mid<-sapply(d_tid_mid,"[",3) 
  ####      (as.numeric(tid)+1)   预估的时间段是训练的时间段加1  为了提交的时候匹配  
  
  #head(cbind(te_lable,wpr))
  te_lable_new<-te_lable[te_lable>0]
  wpr<-wpr[te_lable>0]
  
  finalerr<-sum(abs(te_lable_new-wpr)/te_lable_new)/(66*5)
  cat("++++++++++:i:<",i,">  final loss: ",finalerr,"\n") 
}  




pr_all<<-as.numeric()
te_lable_all<<-as.numeric()
wfile_all<<-NULL

for(i in c(46,58,70,82,94,106,118,130,142)){
  
  i<-(i-offlineflag)
  tr_lable<-trbak[[i]]$y
  #size<-dim(trbak[[i]])[2]
  trdata<-trbak[[i]][,-c(1:cutfild)]
  
  te_lable<-tebak[[i]]$y
  
  tedata<-tebak[[i]][,-c(1:cutfild)]
  
  #m.test <- hashed.model.matrix(f, tedata, 2^20)
  #wpr<-predict(marsTuned[[i]],m.test)
  
  
  wpr<-predict(marsTuned[[i]],tedata)
  wpr<-as.matrix(wpr)
  #  marsTuned[[i]] <- enet(x = as.matrix(trdata), y = tr_lable,
  #                     lambda = 0.001)
  #  pr <- predict(marsTuned[[i]], newx = as.matrix(tedata),
  #                 s = 1, mode = "fraction", type = "fit")
  #  wpr<-pr$fit
  wpr[wpr<0]<-0
  
  te_lable1<-te_lable[te_lable>0]
  wpr1<-wpr[te_lable>0]
  finalerr<-sum(abs(te_lable1-wpr1)/te_lable1)/(66*5)
  
  cat("++++++++++++++++++++++:i:<",i,">        final loss: ",finalerr,"\n") 
  
  te_lable_all<-rbind(te_lable_all,cbind(te_lable))
  pr_all<-rbind(pr_all,wpr)
  
  tid<-(i+1)
  mid<-tebak[[i]]$yXstart_mid
  day<-tebak[[i]]$yXday
  day_tid<-as.character(paste(day,tid,sep = "-"))
  wpr<-as.numeric(wpr)
  wfile<-as.data.frame(cbind(mid,day_tid,wpr))
  wfile_all<-rbind(wfile_all,wfile)
  
}

#head(cbind(te_lable,wpr))
te_lable_new<-te_lable_all[te_lable_all>0]
pr_all<-pr_all[te_lable_all>0]

finalerr<-sum(abs(te_lable_new-pr_all)/te_lable_new)/(66*9*5)
cat("++++++++++++++++++++++:total  final loss: ",finalerr,"\n") 

if((offlineflag==1))
{
  write.csv(wfile_all, file =paste(OUTPUT_DIR,"/ALL_offline_test.csv",sep = ""), quote=FALSE, row.names=FALSE)
}else{
  write.csv(wfile_all, file =paste(OUTPUT_DIR,"/ALL_updata.csv",sep = ""), quote=FALSE, row.names=FALSE)
}

############查看模型   一共就9个模型"46","58","70","82","94","106","118","130","142"
##看  预估时间片为70的小模型参数
#i<-142
# str  这个信息量大
# str(marsTuned[[i]])
#summary(marsTuned[[i]])
#marsTuned[[i]]
