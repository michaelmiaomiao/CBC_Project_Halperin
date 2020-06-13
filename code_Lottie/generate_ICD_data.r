library(dplyr)
library(tidyr)

#source data
source("/u/project/sriram/ukbiobank/33127/bd_load_data.R")
db<-data

#select columns with ICD code
ICD_var<- c('40006-0.0','40013-0.0','41202-0.0','41203-0.0','41204-0.0',
           '41205-0.0','41270-0.0','41271-0.0')
ICD<- db %>% select(c('eid',ICD_var))%>% mutate_all(na_if,"") 

#select ICD codes with fewer missing values
sapply(ICD,function(x) sum(!is.na(x))/length(x))
ICD <- ICD %>% select(c('eid','41202-0.0','41204-0.0','41270-0.0'))
ICD202<-  ICD %>% select(c('eid','41202-0.0'))
ICD270<-  ICD %>% select(c('eid','41270-0.0'))
ICD204<-  ICD %>% select(c('eid','41204-0.0'))

#ICD categories
ICD10<-read.csv('phecode_icd10.csv')
ICD10$ICD10<-gsub("\\.","",ICD10$ICD10)
ICD10<- ICD10 %>% mutate_all(na_if,"") 

#Match codes in the dataset with categories
ICD202_withlabel<- merge(ICD202,ICD10[,c('ICD10','Excl..Phenotypes')], by.x = '41202-0.0',by.y = 'ICD10',all.x = TRUE)
ICD202_withlabel <- droplevels(ICD202_withlabel)
ICD202_withlabel<- ICD202_withlabel %>% filter(!is.na(`41202-0.0`))
table_202<-as.data.frame(table(ICD202_withlabel$Excl..Phenotypes))
table_202<-table_202[order(table_202$Freq,decreasing =TRUE),]

ICD270_withlabel<- merge(ICD270,ICD10[,c('ICD10','Excl..Phenotypes')], by.x = '41270-0.0',by.y = 'ICD10',all.x = TRUE)
ICD270_withlabel <- droplevels(ICD270_withlabel)
ICD270_withlabel<- ICD270_withlabel %>% filter(!is.na(`41270-0.0`))
table_270<-as.data.frame(table(ICD270_withlabel$Excl..Phenotypes))
table_270<-table_270[order(table_270$Freq,decreasing =TRUE),]

ICD204_withlabel<- merge(ICD204,ICD10[,c('ICD10','Excl..Phenotypes')], by.x = '41204-0.0',by.y = 'ICD10',all.x = TRUE)
ICD204_withlabel <- droplevels(ICD204_withlabel)
ICD204_withlabel<- ICD204_withlabel %>% filter(!is.na(`41204-0.0`))
table_204<-as.data.frame(table(ICD204_withlabel$Excl..Phenotypes))
table_204<-table_204[order(table_204$Freq,decreasing =TRUE),]

#select disease category with higher frequency
c1<-table_202 %>% filter(Freq>10000) %>% select(Var1)
c2<-table_270 %>% filter(Freq>10000) %>% select(Var1)
c3<-table_204 %>% filter(Freq>10000) %>% select(Var1)
var<-unique(c(as.character(c1$Var1),as.character(c2$Var1),as.character(c3$Var1)))

#get dataset with ICD code
ICD_withlabel<- merge(ICD,ICD10[,c('ICD10','Excl..Phenotypes')], by.x = '41202-0.0',by.y = 'ICD10',all.x = TRUE)
ICD_withlabel<- merge(ICD_withlabel,ICD10[,c('ICD10','Excl..Phenotypes')], by.x = '41270-0.0',by.y = 'ICD10',all.x = TRUE)
ICD_withlabel<- merge(ICD_withlabel,ICD10[,c('ICD10','Excl..Phenotypes')], by.x = '41204-0.0',by.y = 'ICD10',all.x = TRUE)

ICD_withlabel<-ICD_withlabel %>% filter(Excl..Phenotypes.x %in% var | Excl..Phenotypes.y %in% var |Excl..Phenotypes.x %in% var  )
ICD_withlabel<-ICD_withlabel %>% select(-c('41204-0.0','41270-0.0','41202-0.0'))
colnames(ICD_withlabel)<- c('eid','d1','d2','d3')

ICD_withlabel$d1[!ICD_withlabel$d1 %in% var]<-NA
ICD_withlabel$d2[!ICD_withlabel$d2 %in% var]<-NA
ICD_withlabel$d3[!ICD_withlabel$d3 %in% var]<-NA
ICD_withlabel$d1<-factor(ICD_withlabel$d1)
ICD_withlabel$d2<-factor(ICD_withlabel$d2)
ICD_withlabel$d3<-factor(ICD_withlabel$d3)

same<-function(i)
    {
    r<- ICD_withlabel[i,]
    r<-c(as.character(r[,2]),as.character(r[,3]),as.character(r[,4]))
    r<-r[!is.na(r)]
    return(unique(r))
}

result<-as.data.frame(ICD_withlabel$eid)
result$d1<-NA
result$d2<-NA
result$d3<-NA

for(i in 1:dim(result)[1])
    {
    r<-same(i)
    if(length(r)==1)
        {
        r <- c(r,NA,NA)
    }
    else if(length(r) ==2)
        {
        r<-c(r,NA)
    }
    result[i,2:4]<-r
}

write.csv(result,'result.csv',row.names=F)


