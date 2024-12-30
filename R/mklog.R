mklog<-function(name_dataset, select_columns) {
  
  if (base::missing(name_dataset)) {
    cat("  df<-mklog(df, 변수명 또는 컬럼번호)  *NOTE: 변수명(OR 컬럼번호) for natural log-transformation  ",  '\n')
	cat("  \033[1;34m# 변수의 레코드값은 1.0 이상이여야만 합니다. 먼저 min()을 사용해 레코드값을 점검하세요. \033[0m ",  '\n')
    return(cat("  \033[1;34m# 로그값 예시: log(0)=-Inf, log(0.1)=-2.30, log(1.0)=0, log(2)=0.69, log(10)=2.30  \033[0m ") ) }
  
  if(class(name_dataset)!="data.frame") {
	cat("  CORRECT COMMAND: df<-mklog(df, number of column)", '\n')
	cat("  EXAMPLE:         df<-mklog(df, 14)", '\n')
	return(0) }
	
##-----------------------------------
# 변수명을 컬럼 번호로 변경시키는 함수
##------------------------------------
find_col2<-function(DataSet, index_id ){
  tmp_colnames<-colnames(DataSet)
  n<-length(tmp_colnames)  # DataSet의 총변수 갯수
  
  for(i in 1:n){
    if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
  }
}

##----------------------------------------
# find_col2()를 사용해 컬럼번호 찾기
if(is.numeric(select_columns)==F) {select_columns<-find_col2(name_dataset, select_columns) }
##----------------------------------------

  name_dataset<-as.data.frame(name_dataset)
  tmp<-(name_dataset[select_columns])
  
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
      
	  else if(tmp$log_[i]<0 & tmp$log_[i]>-1) {
	  cat("  ", i,"th value is -1<X<0 , No log-transformation",'\n')
	  tmp$log_[i]<-NA
	  }
	  
	  else if(tmp$log_[i]>=1) {tmp$log_[i]<-log(tmp$log_[i])}
	  
          else if(tmp$log_[i] <= -1) {tmp$log_[i]<- ( -1*log(-1*tmp$log_[i]))}

	  else { print('  ** There is problesm in log-transformation  **') }
	  
	  }

  colnames(tmp) <- paste0( colnames(tmp) , colnames(name_dataset[select_columns]) , sep='')
  name_dataset2 <- cbind(name_dataset, tmp)
  return(name_dataset2)

}
