mklog<-function(name_dataset, select_columns) {
 
if (base::missing(name_dataset)) {
cat(" df<-mklog(df, variable) *NOTE: 변수에 자연로그를 취함 ", '\n')
    cat(" \033[1;34m# 변수의 값들은 1.0보다 커야 합니다. min()으로 최솟값을 체크하세요. \033[0m ", '\n')
return(cat(" \033[1;34m# example of logs : log(0)=-Inf, log(0.1)=-2.30, log(1.0)=0, log(2)=0.69, log(10)=2.30 \033[0m ") ) }

if(class(name_dataset)!="data.frame") {
    return(cat(" CORRECT COMMAND: df<-mklog(df, 변수명)", '\n')) }

##------------------------------------
select_columns <- deparse(substitute(select_columns))    

find_col2<-function(DataSet, index_id ){
  tmp_colnames<-colnames(DataSet)
  n<-length(tmp_colnames) # DataSet의 총변수 갯수

    for(i in 1:n){
        if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
    }
}

if(is.numeric(select_columns)==F) {select_columns<-find_col2(name_dataset, select_columns) }
##----------------------------------------

name_dataset<-as.data.frame(name_dataset)
tmp<-(name_dataset[select_columns])
if(min(tmp)<1) {cat(" Warning: some data are less than 1. Stop log-transformation. ", '\n')
    break }

colnames(tmp)<-c("log_")
n<-nrow(tmp)

for(i in 1:n){

     if (is.na(tmp$log_[i])) {tmp$log_[i]<-NA}

     else if(tmp$log_[i]==0) {
     cat("Since there is X==0 case, the log-change can not be used!",'\n')
     return(name_dataset)
     break}

     else if(tmp$log_[i]>0 & tmp$log_[i]<1) {
     cat('Since 0<X<1 case, the log-change can not be used!', '\n')
     return(name_dataset)
     break}

     else if(tmp$log_[i]<0 & tmp$log_[i]>-1) {
     cat('Since -1<X<0 case, the log-change of X can not be used!')
     return(name_dataset)
     break}
    
     else if(tmp$log_[i]>=1) {tmp$log_[i]<-log(tmp$log_[i])}
    
else if(tmp$log_[i] <= -1) {tmp$log_[i]<- ( -1*log(-1*tmp$log_[i]))}
    
     }

colnames(tmp) <- paste0( colnames(tmp) , colnames(name_dataset[select_columns]) , sep='')
name_dataset2 <- cbind(name_dataset, tmp)
return(name_dataset2)

}
