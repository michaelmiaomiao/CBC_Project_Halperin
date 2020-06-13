library(dplyr)
library(tidyr)
library(data.table)
library(mltools)
library(stringr)

#read result dataset from generate_ICD_data.r
result<-read.csv('result.csv')
result <- result[,1:3]
colnames(result)[1]<-'eid'

#make dataset onehot format
result_matrix1<-as.data.frame(one_hot(as.data.table(result[,1:2])))
result1<-result[,c(1,3)] %>% filter(!is.na(d2))
result_matrix2<-one_hot(as.data.table(result1))

#combine same variable
var<-str_split(colnames(result_matrix1)[-1],'_')
var1<-c()
for(i in 1:16){
    var1<-c(var1,var[[i]][2])
}
df <- data.frame(matrix(ncol = 17, nrow = nrow(result)))
colnames(df)<-c('eid',var1)
df$eid<-result$eid
for(i in 2:17)
    {
    df[which(result_matrix1[,i]==1),i]<-1
}
df[which(result_matrix2[,2]==1),2]<-1
df[which(result_matrix2[,3]==1),3]<-1
df[which(result_matrix2[,4]==1),4]<-1
df[which(result_matrix2[,5]==1),5]<-1
df[which(result_matrix2[,6]==1),6]<-1
df[which(result_matrix2[,7]==1),7]<-1
df[which(result_matrix2[,8]==1),8]<-1
df[which(result_matrix2[,9]==1),9]<-1
df[which(result_matrix2[,10]==1),10]<-1
df[which(result_matrix2[,11]==1),11]<-1
df[which(result_matrix2[,12]==1),12]<-1
df[which(result_matrix2[,13]==1),13]<-1
df[which(result_matrix2[,14]==1),14]<-1
df[which(result_matrix2[,15]==1),15]<-1
df[which(result_matrix2[,16]==1),16]<-1
df[which(result_matrix2[,17]==1),17]<-1
df[is.na(df)]<-0

#output ICD data with onehot format
kk<-df %>% select(-eid) %>% mutate_all(funs(factor(.)))
kk$eid <- df$eid
kk<- kk %>% select('eid',everything())
write.csv(kk,'more_feature.csv',row.names=F)
