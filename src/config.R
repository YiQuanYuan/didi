library(caret)
library(data.table)
library(AppliedPredictiveModeling)
library(glmnet)
library(MASS)
library(e1071)
library(kernlab)

daydir<-"20160601"
OUTPUT_DIR<-paste("/Users/baidu/work/didi/updata/",daydir,sep = "")
DATADIR<-"/Users/baidu/work/didi/data"
BINDIR<-"/Users/baidu/work/didi/bin"

cat("################ 加载lib,设置路径\n")



