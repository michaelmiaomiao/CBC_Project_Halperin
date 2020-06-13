library(ggplot2)
library(rvest)
library(dplyr)
library(stringr)

#source data
source("/u/project/sriram/ukbiobank/33127/bd_load_data.R")
my_data<-data

#generate data type
theurl <- "/u/project/sriram/ukbiobank/33127/ukb39967.html"
file<-read_html(theurl)
rank_data_html <- html_nodes(file,'td :nth-child(1)')
rank_data <- html_text(rank_data_html)[-1]
type<-unique(rank_data[which(str_count(rank_data, "[0-9]")==0)])
type<-type[type!='']
id_type_ind<-which(rank_data %in% colnames(my_data) | rank_data %in% type)
id_type<-rank_data[id_type_ind]
id_type_table<-data.frame(eid = id_type[seq(1,length(id_type),by=2)], type = id_type[seq(2,length(id_type),by=2)])

#Pick categorical variables
cate<-c('Categorical (single)' ,'Categorical (multiple)')
cate_cor <- as.character(id_type_table[which(id_type_table$type %in% cate),1])
my_data_cate<- my_data %>% select(cate_cor) %>% mutate_all(funs(factor(.)))
db <- my_data_cate 
db$eid <- my_data$eid
db$WBCC <- my_data$`30000-0.0`
db<- db %>% select(c('eid','WBCC'),everything())

#Summary for White blood cell
db %>% select(WBCC) %>% summarise(mean = mean(WBCC,na.rm=T)
                                 ,median = median(WBCC,na.rm=T)
                                 ,sd = sd(WBCC,na.rm=T)
                                 ,min = min(WBCC,na.rm=T)
                                 ,max = max(WBCC,na.rm=T)
                                 ,missing = sum(is.na(WBCC)))

ggplot(db, aes(x=WBCC)) + geom_histogram(binwidth=2,color="black", fill="white")

#Remove all of the NA of white blood cell count
db <- db%>% filter(!is.na(WBCC))

#Remove columns with too many missing values
missing<-db %>% select(everything()) %>% summarise_all(funs(sum(is.na(.))))
missing<-as.data.frame(t(missing))
missing$var<-rownames(missing)
rownames(missing)<-1:length(missing$V1)
missing[which(missing$V1==0),'var']
remove_var<-missing$var[which(missing$V1 > dim(db)[1]*0.5)]
db <- db %>% select(-remove_var)

#Remove columns with too many levels
cols<-data.frame(var = setdiff(names(db), c('eid',"WBCC"))) 
cols$var <-as.character(cols$var)
col_names<-read.csv('col_names.csv')
cols$var_names<-col_names$var_names
cols$p<-NA
db_levels<-as.data.frame(sapply(db,nlevels))
rm_fac<-rownames(db_levels)[which(db_levels[,1]>50 | db_levels[,1]==1)]
db<-  db %>% select(-rm_fac)
cols<-cols %>% filter( ! var %in% rm_fac)

#Calculate p-values
anova_p<-function(name){
    w<- paste('`',name,'`',sep = '')
    formula <- lm(as.formula(paste( "WBCC ~ ",w)),db)
    return(anova(formula)[1,5])
    }

for(i in 1:length(cols$var)){
    cols$p[i]<-anova_p(cols$var[i])
}

#Remove variables with big p-values
rm_p<-cols$var[which(cols$p> 10^-8)]
db<-  db %>% select(-rm_p)
cols<-cols %>% filter( ! var %in% rm_p)

#boxplots
plot_box<-function(var,var_name){
    print(ggplot(aes(x = get(var),y=WBCC),data = db)+ geom_boxplot()+ylim(0,25)+xlab(var_name))
}
options(repr.plot.width=5, repr.plot.height=4)
for( i in 1:length(cols$var))
    {
    plot_box(cols$var[i],cols$var_names[i])
}

#based on boxplots and p-values, delete some variables
delete<-c('21-0.0','54-0.0','55-0.0','120-0.0','670-0.0','680-0.0','728-0.0','738-0.0','1031-0.0','1110-0.0')
cols<-cols %>% filter( ! var %in% delete)
delete<-c('1647-0.0','1677-0.0','1687-0.0','1697-0.0','1717-0.0','1727-0.0','1747-0.0','1757-0.0','1767-0.0','1797-0.0')
cols<-cols %>% filter( ! var %in% delete)
delete<-c('1835-0.0','2237-0.0','2296-0.0','3393-0.0','4081-0.0','6138-0.0','6141-0.0','6142-0.0','6143-0.0','20118-0.0')
cols<-cols %>% filter( ! var %in% delete)
delete<-c('30004-0.0','30014-0.0','40020-0.0','40021-0.0','40022-0.0','41244-0.0','41247-0.0','41249-0.0','41250-0.0','41253-0.0')
cols<-cols %>% filter( ! var %in% delete)

#write out variables
write.csv(cols,'columns.csv',row.names = F)
