



system.time(avg_updata_1<-fread(paste(OUTPUT_DIR,"/MARS.csv",sep = ""), sep = ","))
system.time(avg_updata_2<-fread(paste(OUTPUT_DIR,"/avNNet.csv",sep = ""), sep = ","))
system.time(avg_updata_3<-fread(paste(OUTPUT_DIR,"/updata_randomForest.csv",sep = ""), sep = ","))

tag<-c("a1","a2","a3")
names(avg_updata_1)<-tag
tag<-c("b1","b2","b3")
names(avg_updata_2)<-tag
tag<-c("c1","c2","c3")
names(avg_updata_3)<-tag

avg_updata<-cbind(avg_updata_1,avg_updata_2,avg_updata_3)
avg_updata<-as.data.frame(avg_updata)

avg_updata$avg_up<-(avg_updata$a3+avg_updata$b3+avg_updata$c3)/3



avg_updata$a3<-NULL
avg_updata$b1<-NULL
avg_updata$b2<-NULL
avg_updata$b3<-NULL
avg_updata$c1<-NULL
avg_updata$c2<-NULL
avg_updata$c3<-NULL

write.csv(avg_updata, file =paste(OUTPUT_DIR,"/avg_updata.csv",sep = ""), quote=FALSE, row.names=FALSE)


