na_col <-function(dataset){
  n<-ncol(dataset)
  for(i in 1:n){
    tmp.no<-sum(is.na(df[,c(i)]))
    if( tmp.no >0 ) {
	cat( colnames(dataset)[i], ' : ')
	cat( '' , tmp.no )
	cat("\033[1;31m *** \033[0m", '\n')}
    else( cat( colnames(dataset)[i], ': ', sum(is.na(df[,c(i)])), '\n') )
  }
}
