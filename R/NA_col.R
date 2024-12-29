NA_col <-function(dataset){
  n<-ncol(dataset)
  for(i in 1:n){
    if( sum(is.na(df[,c(i)])) >0 ) {cat( colnames(dataset)[i],':' ,sum(is.na(df[,c(i)])) , '\n')}
    else( cat( colnames(dataset)[i], ': ', sum(is.na(df[,c(i)])), '\n') )
  }
}
