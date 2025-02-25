mkdum <- function(name_dataset, select_columns) {
  
  if (base::missing(name_dataset)) {
	    cat("  df<-as.data.frame(df)", '\n')
        return( cat("  df<-mkdum(df, 2)  *NOTE: making dummies for brand(2nd column of dataset) ", '\n') )
	}
  
  if(class(name_dataset)!="data.frame") {
	cat("  CORRECT COMMAND: df<-mkdum(data.frame, number of column) ", '\n')
	cat("  EXAMPLE:         df<-mkdum(data.frame, 3)", '\n')
	return(0) }
	
  if(class(select_columns)!="numeric") {
	cat("  CORRECT COMMAND: df<-mkdum(data.frame, number of column)", '\n')
	cat("  EXAMPLE:         df<-mkdum(data.frame, 3)", '\n')
	return(0) }

  name_dataset<-as.data.frame(name_dataset)
  tmp<-(name_dataset[select_columns])
  tmp[is.na(tmp)]<-"NA"
  colnames(tmp)<-c("dum_")
  tmp$dum_ <- as.factor( tmp$dum_ )
    
  ## if the codes of variable is only two types, 
  ## directly make dummy variables by using following method.
  if( length(unique(tmp$dum_))==2 ) {
    cat("  SUGGESTED COMMANDS: df$gender2[df$gender==1]<-1; df$gender2[df$gender==2]<-0 ", '\n')
	return(name_dataset)
	break}
  else {tmp_select_columns <- model.matrix(~ dum_ -1, tmp)}
   
  ## Changing variable names
  colnames(tmp_select_columns) <- paste0( colnames(name_dataset[select_columns]) , colnames(tmp_select_columns) , sep='')
  
  name_dataset2<-cbind(name_dataset, tmp_select_columns)
  name_dataset2<-as.data.frame(name_dataset2)
  return(name_dataset2)
}

