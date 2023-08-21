mklog<-function(name_dataset, select_columns) {
  
  if (base::missing(name_dataset)) {
    cat("  df<-as.data.frame(df) ", '\n')
    return(cat("  df<-mklog(df, 14)  *NOTE: 14 = 14th variable for log-transformation  ") ) }
  
  if(class(name_dataset)!="data.frame") {
	cat("  CORRECT COMMAND: df<-mklog(data.frame, number of column)", '\n')
	cat("  EXAMPLE:         df<-mklog(data.frame, 14)", '\n')
	return(0) }
	
  if(class(select_columns)!="numeric") {
	cat("  CORRECT COMMAND: df<-mklog(data.frame, number of column)", '\n')
	cat("  EXAMPLE:         df<-mklog(data.frame, 3)", '\n')
	return(0) }
     
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
