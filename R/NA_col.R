na_col <-function(dataset){

  if (base::missing(dataset)) {
  cat("  # 각 변수에 NA미싱데이터의 갯수를 체크 ------ ", '\n')
  return(cat("    na_col(df) "))}

  df <- data.frame(dataset)
  n<-ncol(dataset)


  for(i in 1:n){
    tmp.no<-sum(is.na(df[, i ]))
    if( tmp.no >0 ) {
	cat( colnames(dataset)[i], ' : ')
	cat( '' , tmp.no )
	cat("\033[1;31m *** \033[0m", '\n')}
    else( cat( colnames(dataset)[i], ': ', sum(is.na(df[ ,i ])), '\n') )
  }
}
