library(ggplot2)
library(caTools)

#source data
source("/u/project/sriram/ukbiobank/33127/bd_load_data.R")
db<-data

#select significant variable from model_without_ICD.r and some data processing
db$WBCC <- db$`30000-0.0`
db<- db %>% filter(!is.na(WBCC))
var<-c('31-0.0','1558-0.0','1239-0.0','1249-0.0','2100-0.0','21000-0.0')
db1<- db %>% select(var)
db1[which(db1$`21000-0.0`<1010 & db1$`21000-0.0`>1000),'21000-0.0']<-1
db1[which(db1$`21000-0.0`<2010 & db1$`21000-0.0`>2000),'21000-0.0']<-2
db1[which(db1$`21000-0.0`<3010 & db1$`21000-0.0`>3000),'21000-0.0']<-3
db1[which(db1$`21000-0.0`<4010 & db1$`21000-0.0`>4000),'21000-0.0']<-4
db1[which(db1$`21000-0.0`==5),'21000-0.0']<-3
db1 <- db1 %>% mutate_all(na_if,"") 
db1<- db1 %>% mutate_each(funs(replace(., .<0, NA)))
db1<- db1 %>% mutate_all(funs(factor(.)))
db1$eid<-db$eid
db1$WBCC <- db$WBCC
db1<- db1 %>% select(c('eid','WBCC'),everything())

#add more_feature from rearrange_ICD_data.r
disease<-read.csv('more_feature.csv')
disease1<- disease[,-1] %>% mutate_all(funs(factor(.)))
disease1$eid<-disease$eid
disease1<- disease1 %>% select('eid',everything())
df<-merge(db1,disease1,by='eid')
df1<- df%>% select(-c('eid','WBCC')) %>% mutate_all(funs(droplevels(.)))
df1$eid<-df$eid
df1$WBCC<-df$WBCC
df1<-na.omit(df1)

#train and test data
set.seed(1233) 
sample = sample.split(df1,SplitRatio = 0.7) 
train1 =subset(df1,sample ==TRUE)
test1=subset(df1, sample==FALSE)
model1<-aov(WBCC~.  -eid -Cataract -Malignant.neoplasm.of.skin -mononeuritis.and.nerve.lesions -Anemias-DISORDERS.OF.THYROID.GLAND-`2100-0.0`-Benign.and.malignant.neoplasm.of.colon, data =train1)
summary(model1)
hh<-predict(model1, test1)
SSR<-sum((hh-mean(test1$WBCC))^2)
SST<-sum((test1$WBCC-mean(test1$WBCC))^2)
SSR/SST
