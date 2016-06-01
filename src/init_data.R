makenewdata<-0
if(makenewdata==1)
{
  source("/Users/baidu/work/didi/bin/didi_config.R")
  source(paste(BINDIR,"didi_lib.R",sep = ""))
  
  
  ##########################init data
  cat("################ 读取order原始数据\n")
  system.time(ordata<-fread(paste(DATADIR,"/original_data",sep=""), sep = " "))
  ######给数据列  加上名字 方便引用
  
  tag=c("order_id","driver_id","passenger_id","start_district_hash","dest_district_hash","Price","day","hour","timeid")
  names(ordata)<-tag
  
  cat("################ 读取map 映射原始数据\n")
  cluster_map<-fread(paste(DATADIR,"/cluster_map",sep=""), sep = " ")
  
  cat("################ 给数据列  加上名字 方便引用\n")
  tag<-c("district_hash","district_id")
  names(cluster_map)<-tag
  
  ###  把地域的 id  left join 到原始表里
  cat("################ 把地域的 id  left join 到原始表里\n")
  ordata_map<-merge(ordata,cluster_map,by.x="start_district_hash",by.y = "district_hash",all.x = TRUE)
  ordata<-merge(ordata_map,cluster_map,by.x="dest_district_hash",by.y = "district_hash",all.x = TRUE)
  rm(ordata_map)
  gc()###清一下缓存
  ordata<-as.data.frame(ordata)
  
  tag<-c("start_district_hash","dest_district_hash","order_id","driver_id","passenger_id","Price","day","hour","timeid","start_mid","dest_mid")
  names(ordata)<-tag
  
  #######提取想用的列
  
  
  cat("################ 提取想用的列\n")
  select_filed<-c("day","timeid","start_mid","dest_mid","Price","order_id","driver_id","passenger_id")
  ordata<-ordata[,select_filed]
  gc()
  dim(ordata)
  dim(ordata[is.na(ordata[,"dest_mid"]),])
  ################### 目的地  是未知区域的 赋值为100 
  cat("################ 目的地  是未知区域的 赋值为100 \n")
  ordata$dest_mid[is.na(ordata$dest_mid)]<-100
  ### 目的地不为空的一共有: 9098599-1437913
  
  ############################### 时间分片处理的时候发现有0的,shell,数据处理弄错了,修补下
  ##    dest_district_hash           Price  day       hour        timeid
  ##90c5a34f06ac86aee0fd70e2adce7d8a    11 2016-01-01 00:00:40      0
  ##2308e5edf2fbee52203b5c262f557ddf    18 2016-01-01 00:00:57      0
  
  ordata[ordata$timeid==0,"timeid"]<-1
  
  ##增加一个统计字段   为了能汇总 driver id 为空的值
  ordata$driver_id_null<-0
  ordata[ordata$driver_id=="NULL","driver_id_null"]<-1
  # dim(ordata)   9098599      9
  ##增加一个统计字段   为了能计算汇总后,每个分组内原始记录个数,方便求均值
  ordata$count_ordernum<-1
  cat("################ day ,timeid,startid 用冒号分割加成一列\n")
  
  #ordatabka<-ordata
  
  ############## 去掉用户好几次都没叫到车情况
  ordata$del_user_rep<-paste(ordata$day,ordata$timeid,ordata$start_mid,ordata$passenger_id,ordata$driver_id_null,sep = "z")
  
  ordata<-as.data.table(ordata)
  
  ordata_a<-ordata[which(!(day=="2016-01-21"|day=="2016-01-20"|day=="2016-01-19"|day=="2016-01-18"|day=="2016-01-17"|day=="2016-01-16"|day=="2016-01-15"|day=="2016-01-14"|
                             day=="2016-01-22"|day=="2016-01-24"|day=="2016-01-26"|day=="2016-01-28"|day=="2016-01-30")),]
  ordata_b<-ordata[which((day=="2016-01-21"|day=="2016-01-20"|day=="2016-01-19"|day=="2016-01-18"|day=="2016-01-17"|day=="2016-01-16"|day=="2016-01-15"|day=="2016-01-14"|
                            day=="2016-01-22"|day=="2016-01-24"|day=="2016-01-26"|day=="2016-01-28"|day=="2016-01-30")),]
  
  ################  
  ordata_a$del_user_rep<-gsub(pattern = "-", replacement = "z", x = ordata_a$del_user_rep)
  ordata_a<-as.data.frame(ordata_a)
  ordata_a<-ordata_a[order(ordata_a[,"del_user_rep"],decreasing = TRUE),]
  ordata_a<-as.data.table(ordata_a)
  ordata_a<-unique(ordata_a, by="del_user_rep")
  ####################
  ordata_b$del_user_rep<-gsub(pattern = "-", replacement = "z", x = ordata_b$del_user_rep)
  ordata_b<-as.data.frame(ordata_b)
  ordata_b<-ordata_b[order(ordata_b[,"del_user_rep"],decreasing = TRUE),]
  ordata_b<-as.data.table(ordata_b)
  ordata_b<-unique(ordata_b, by="del_user_rep")
  #####################
  ordata_a$del_user_rep_1<-paste(ordata_a$day,ordata_a$timeid,ordata_a$start_mid,ordata_a$passenger_id,sep = "z")
  ordata_a$del_user_rep_1<-gsub(pattern = "-", replacement = "z", x = ordata_a$del_user_rep_1)
  ordata_a<-unique(ordata_a, by="del_user_rep_1")
  #####################
  ordata_b$del_user_rep_1<-paste(ordata_b$day,ordata_b$timeid,ordata_b$start_mid,ordata_b$passenger_id,sep = "z")
  ordata_b$del_user_rep_1<-gsub(pattern = "-", replacement = "z", x = ordata_b$del_user_rep_1)
  ordata_b<-unique(ordata_b, by="del_user_rep_1")
  ##################   (dim(ordatabka)[1]-dim(ordata)[1])/dim(ordata)
  ##################   原始 9098599   去重后 7663345 
  ###################一共去掉了1435254 一百多万条数据 ,占 18.7%
  ordata<-rbind(ordata_a,ordata_b)
  ##############
  #ordatabka<-ordata
  ordata$del_user_rep_1<-NULL
  ordata$del_user_rep<-NULL
  
  write.csv(ordata, file =paste(DATADIR,"/newdata_del_user",sep=""), quote=FALSE, row.names=FALSE)
}

ordata$str_merge<-paste(ordata$day,ordata$timeid,ordata$start_mid,sep = ":")
########################################################
attach(ordata)###这个函数意思是要用 aggregate  做 group by 前  先让 R 知道列是啥... 
##  对 day,timeid,start_mid  维度下,为 null 的司机数,以及价格,求和.
cat("## 对day:timeid:出发地点 +到达地 做 group by 对价格,gap,订单数,求和\n")
gather<-aggregate(cbind(driver_id_null,Price,count_ordernum) ~ str_merge+dest_mid, FUN=sum)
gather$av_price<-gather$Price/gather$count_ordernum
#head(gather)

##   
cat("## 对day:timeid:出发地点(没到达地点)做 group by 对价格,gap,订单数,求和\n")
gather_no_destmid<-aggregate(cbind(driver_id_null,Price,count_ordernum) ~ str_merge+day+timeid+start_mid, FUN=sum)
##增加平均价
cat("## 对 day,timeid,出发地,求均价:sum(price)/订单数  \n")
gather_no_destmid$av_price<-gather_no_destmid$Price/gather_no_destmid$count_ordernum
head(gather_no_destmid)
detach(ordata)

write.csv(gather_no_destmid, file =paste(DATADIR,"/gather_no_destmid",sep=""), quote=FALSE, row.names=FALSE)

###################################################
attach(gather) 
###tapply  相当于 excel 数据透视表
cat("## 数据展开 维度是67个地域,指标是哪天,")
cat("   哪个时间片,哪个地点出发,转移到这个地域的 gap, 建议 head 看下数据\n")
gather_map<-tapply(driver_id_null,list(str_merge,dest_mid),FUN = sum)
head(gather_map)
detach(gather)
rm(gather)
gc()



gather_map[is.na((gather_map))]<-(-1)
head(gather_map)
#########################################################
gather_map<-as.data.frame(gather_map)
########### tapply 转换完把 str_merge 变为行的名字了,  把 str_merge 从行的名字变为一列
gather_map<-cbind(row.names(gather_map),gather_map)
tag<-c("str_merge","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","100")
names(gather_map)=tag
write.csv(gather_map, file =paste(DATADIR,"/gather_map",sep=""), quote=FALSE, row.names=FALSE)


#####################################################
###统计司机数量
###统计用户数
ordatabak<-ordata
ordata$cont_indepant_driver<-paste(ordata$day,ordata$timeid,ordata$start_mid,ordata$driver_id,sep = "z")

ordata$cont_indepant_driver<-gsub(pattern = "-", replacement = "z", x = ordata$cont_indepant_driver)
ordata<-as.data.frame(ordata)
ordata<-ordata[order(ordata[,"cont_indepant_driver"],decreasing = TRUE),]
ordata<-as.data.table(ordata)
ordata<-unique(ordata, by="cont_indepant_driver")
##  (7663345-6380635)/6380635   司机去重之后减少20%

attach(ordata)###这个函数意思是要用 aggregate  做 group by 前  先让 R 知道列是啥... 
##  对 day,timeid,start_mid  维度下,为 null 的司机数,以及价格,求和.
cat("## 对day:timeid:出发地点 +到达地 做 group by 对价格,gap,订单数,求和\n")
gather_drv_cont<-aggregate(cbind(count_ordernum) ~ str_merge+dest_mid, FUN=sum)
detach(ordata)

attach(gather_drv_cont) 
###tapply  相当于 excel 数据透视表
cat("## 数据展开 维度是67个地域,指标是哪天,")
cat("   哪个时间片,哪个地点出发,转移到这个地域的 gap, 建议 head 看下数据\n")
gather_drv_map<-tapply(count_ordernum,list(str_merge,dest_mid),FUN = sum)
head(gather_drv_map)
detach(gather_drv_cont)

gather_drv_map<-as.data.frame(gather_drv_map)
########### tapply 转换完把 str_merge 变为行的名字了,  把 str_merge 从行的名字变为一列
gather_drv_map<-cbind(row.names(gather_drv_map),gather_drv_map)
tag<-c("str_merge","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","100")
names(gather_drv_map)=tag
write.csv(gather_drv_map, file =paste(DATADIR,"/gather_drv_map",sep=""), quote=FALSE, row.names=FALSE)

#tmp<-gather_map[is.na((gather_map))]

#####################################################

#####################################################
###统计用户数
ordata$cont_indepant_user<-paste(ordata$day,ordata$timeid,ordata$start_mid,ordata$passenger_id,sep = "z")

ordata$cont_indepant_user<-gsub(pattern = "-", replacement = "z", x = ordata$cont_indepant_user)
ordata<-as.data.frame(ordata)
ordata<-ordata[order(ordata[,"cont_indepant_user"],decreasing = TRUE),]
ordata<-as.data.table(ordata)
ordata<-unique(ordata, by="cont_indepant_user")
##  用户去重后值未变   7663345

attach(ordata)###这个函数意思是要用 aggregate  做 group by 前  先让 R 知道列是啥... 
##  对 day,timeid,start_mid  维度下,为 null 的司机数,以及价格,求和.
cat("## 对day:timeid:出发地点 +到达地 做 group by 对价格,gap,订单数,求和\n")
gather_user_cont<-aggregate(cbind(count_ordernum) ~ str_merge+dest_mid, FUN=sum)
detach(ordata)

attach(gather_user_cont) 
###tapply  相当于 excel 数据透视表
cat("## 数据展开 维度是67个地域,指标是哪天,")
cat("   哪个时间片,哪个地点出发,转移到这个地域的 gap, 建议 head 看下数据\n")
gather_ueser_map<-tapply(count_ordernum,list(str_merge,dest_mid),FUN = sum)
head(gather_ueser_map)
detach(gather_user_cont)

gather_ueser_map<-as.data.frame(gather_ueser_map)
########### tapply 转换完把 str_merge 变为行的名字了,  把 str_merge 从行的名字变为一列
gather_ueser_map<-cbind(row.names(gather_ueser_map),gather_ueser_map)
tag<-c("str_merge","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","100")
names(gather_ueser_map)=tag


write.csv(gather_ueser_map, file =paste(DATADIR,"/gather_ueser_map",sep=""), quote=FALSE, row.names=FALSE)



#####################################################

##############以天,时间片,区域,left join, 把平均价格和 gap 关联过来
cat("################ 以天,时间片,区域,left join, 把平均价格和 gap 关联过来 \n")
data<-merge(gather_map,gather_no_destmid,by.x="str_merge",by.y = "str_merge",all.x = TRUE)
##### 价格可以用 av_price 和 count_ordernum  求的,是线性相关的 就不要了
data$Price<-NULL
rm(ordata)
write.csv(data, file =paste(DATADIR,"/data20160529",sep=""), quote=FALSE, row.names=FALSE)
#
system.time(data<-fread(paste(DATADIR,"/data20160529",sep=""), sep = ",",row.names(c("str_merge","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","100","day","timeid","start_mid","driver_id_null","count_ordernum","av_price"))))
names(data)<-c("str_merge","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","100","day","timeid","start_mid","driver_id_null","count_ordernum","av_price")

system.time(dayv<-fread("/Users/baidu/work/didi/data/day.csv", sep = ","))
data<-merge(data,dayv,by.x="day",by.y = "day",all.x = TRUE)


#tmp<-head(data,100)

#tmp<-as.data.frame(tmp)
#tmp[,c("55","56")]<-tmp[,c("55","56")]*tmp[,c("v2")]

system.time(dayw<-fread("/Users/baidu/work/didi/data/dayweek.csv", sep = ","))
#data<-merge(data,dayv,by.x="day",by.y = "day",all.x = TRUE)


system.time(poi<-fread("/Users/baidu/work/didi/data/poi_data", sep = ","))

cat("################ 读取map 映射原始数据\n")
cluster_map<-fread(paste(DATADIR,"/cluster_map",sep=""), sep = " ")

cat("################ 给数据列  加上名字 方便引用\n")
tag<-c("district_hash","district_id")
names(cluster_map)<-tag

poi_map<-merge(cluster_map,poi,by.x="district_hash",by.y="hash_mip",all.x = TRUE)
poi_map$district_hash<-NULL


weather_a<-fread(paste(DATADIR,"/weather_train.txt",sep=""), sep = ",")
weather_b<-fread(paste(DATADIR,"/weather_test.txt",sep=""), sep = ",")
weather<-rbind(weather_a,weather_b)

tag<-c("day_tid","weather","temper","PM25")
names(weather)<-tag

