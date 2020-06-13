library(ggplot2)
library(caTools)

#source data and select certain columns
source("/u/project/sriram/ukbiobank/33127/bd_load_data.R")
cols<-read.csv('columns.csv',stringsAsFactors = F)
db<-data

db$WBCC <- db$`30000-0.0`
db<- db %>% filter(!is.na(WBCC))

db1 <- db %>% select(cols$var)%>% mutate_all(na_if,"") 
db1<- db1 %>% mutate_each(funs(replace(., .<0, NA)))
db1<- db1 %>% mutate_all(funs(factor(.)))
db1$WBCC <- db$WBCC
db1<- db1 %>% select('WBCC',everything())

db_levels<-as.data.frame(sapply(db1,nlevels))
rm_fac<-rownames(db_levels)[which(db_levels[,-1]==1)]
db1<- db1 %>% select(-rm_fac)

missing<-db1 %>% select(everything()) %>% summarise_all(funs(sum(is.na(.))))
missing<-as.data.frame(t(missing))
missing$var<-rownames(missing)
rownames(missing)<-1:length(missing$V1)
miss_var<-missing[which(missing$V1>200000),'var']
db1<- db1 %>% select(-miss_var)

train1<- na.omit(db1)
train1 <- droplevels(train1)
db_levels<-as.data.frame(sapply(train1,nlevels))
rm_fac<-rownames(db_levels)[which(db_levels[,1]==1)]
db2<- db1 %>% select(-rm_fac)

#build model
mod1<-aov(WBCC~.-`30024-0.0`-`30034-0.0`-`30602-0.0`-`30666-0.0`-`30716-0.0`-`30806-0.0`,db2)
summary(mod1)

#remove insignificant variables and build models
var<-c('WBCC','924-0.0','1100-0.0','1448-0.0','1518-0.0',
       '1190-0.0','2178-0.0','2316-0.0','2443-0.0','21000-0.0',
       '30024-0.0','30602-0.0','30666-0.0','30716-0.0','30806-0.0')
db3<- db2 %>% select(var)
db3<-na.omit(db3)
mod2<-aov(WBCC~.,db3)
summary(mod2)


#train test split
set.seed(333) 
sample = sample.split(db3,SplitRatio = 0.75) 
train1 =subset(db3,sample ==TRUE)
test1=subset(db3, sample==FALSE)

#cross validation
var <- c('31-0.0','1239-0.0','1558-0.0','1249-0.0','2443-0.0','6150-0.0')
train1<-train1%>% select(c('WBCC',var))
mod3<-aov(WBCC~.,train1)
summary(mod3)
pred<-predict(mod3, test1)
SSR<-sum((pred-mean(test1$WBCC))^2)
SST<-sum((test1$WBCC-mean(test1$WBCC))^2)
SSR/SST
