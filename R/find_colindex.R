find_colindex<-function(DataSet, index_id){

  if (base::missing(DataSet)) {
    return(cat("  find_colindex(DataSet, c(vname1, vname2,.. ) ) " ))  }
  
  tmp<-DataSet
  rm(DataSet)
  
  tmp_colnames<-colnames(tmp)
  n<-length(tmp_colnames)  # DataSet의 총변수 갯수
  index_n<-length(index_id)  # 연구자가 선택한 변수명들의 갯수
  r=c()
  
  for(j in 1:index_n){
        for(i in 1:n){
           if(index_id[j]==tmp_colnames[i]) {r=c(r,i)}
        }
  }

cat('  Column Names: c(',index_id,')','\n')
cat('  Column Numbers: c(',r,')','\n')

}
