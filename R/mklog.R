mklog<-function(name_dataset, select_columns) {
  
  if (base::missing(name_dataset)) {
    cat("  df<-as.data.frame(df) ", '\n')
	cat("  df<-mklog(df, 14)  *NOTE: 14 = 14th variable for natural log-transformation  ",  '\n')
	cat("  \033[1;34m# 14번째 변수의 레코드값은 1.0 이상이여야만 합니다. min()을 사용해 레코드값을 점검하세요. \033[0m ",  '\n')
    return(cat("  \033[1;34m# 로그값 예시: log(0)=-Inf, log(0.1)=-2.30, log(1.0)=0, log(2)=0.69, log(10)=2.30  \033[0m ") ) }
  
  if(class(name_dataset)!="data.frame") {
	cat("  CORRECT COMMAND: df<-mklog(data.frame, number of column)", '\n')
	cat("  EXAMPLE:         df<-mklog(data.frame, 14)", '\n')
	return(0) }
	
  if(class(select_columns)!="numeric") {
	cat("  CORRECT COMMAND: df<-mklog(data.frame, number of column)", '\n')
	cat("  EXAMPLE:         df<-mklog(data.frame, 3)", '\n')
	return(0) }
  name_dataset<-as.data.frame(name_dataset)
  tmp<-(name_dataset[select_columns])
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
