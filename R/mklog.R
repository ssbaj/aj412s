mklog <-function(dataset_name , ...) {

## -------------------------------------
if (base::missing(dataset_name)) {
cat(" df<-mklog(df, variable) *NOTE: 변수에 자연로그를 취함 ", '\n')
    cat(" \033[1;34m# 변수의 값들은 1.0보다 커야 합니다. min()으로 최솟값을 체크하세요. \033[0m ", '\n')
return(cat(" \033[1;34m# example of logs : log(0)=NA, log(0.1)=NA, log(1.0)=0, log(10)=2.30 \033[0m ") ) }

## -------------------------------------

# find_col2함수 ----------------
find_col2<-function(dataset_name, index_id ){
  tmp_colnames<-names(dataset_name)
  n<-length(tmp_colnames) # DataSet의 총변수 갯수
    for(i in 1:n){
        if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
    }
  }

##--------------------------------
# 변수명을 읽는 함수
var_names <- as.character(substitute(list(...)))[-1]
no_var_names<-length(var_names)

r=c()

for(i in 1:no_var_names){
  t_counter<-find_col2(dataset_name, var_names[i])
  r=c(r, t_counter)
}

r_var_index_number <- r

##---------------------------------


## log전환 변수는 1개만 가능 ----------------------

makinglogs<-function(dataset_name, select_columns) {
  dataset_name<-as.data.frame(dataset_name)
  tmp<-(dataset_name[select_columns])
  
  colnames(tmp)<-c("log_")
  n<-nrow(tmp)
        
  for(i in 1:n){
     
	  if (is.na(tmp$log_[i])) {tmp$log_[i]<-NA}
      
	  else if(tmp$log_[i]==0) {
	  cat("  ", i,"th value is zero, No log-transformation",'\n')
  	  tmp$log_[i]<-NA
	  }
      
	  else if(tmp$log_[i]>0 & tmp$log_[i]<1) {
	  cat("  ", i,"th value is 0<X<1 , replace it with NA",'\n')
	  tmp$log_[i]<-NA
	  }
      
	  else if(tmp$log_[i]<0 ) {
	  cat("  ", i,"th value is less than 0, No log-transformation",'\n')
	  tmp$log_[i]<-NA
	  }
	  
	  else if(tmp$log_[i]>=1) {tmp$log_[i]<-log(tmp$log_[i])}
	  
	  else { print('  ** There are problems in log-transformation  **') }
	  
	  }

  colnames(tmp) <- paste0( colnames(tmp) , colnames(dataset_name[select_columns]) , sep='')
  return(tmp)
}

## 로그전환 변수를 combin 하기------------------------------------
ncounter_logs <- length(r_var_index_number)

tmplog_dataframe<-dataset_name

for(i in r_var_index_number) {
  tmplog<-makinglogs(dataset_name, i)
  tmplog_dataframe<-data.frame(tmplog_dataframe, tmplog)
}

return(tmplog_dataframe)

}
