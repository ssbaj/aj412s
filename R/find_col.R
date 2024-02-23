find_col<-function(DataSet, index_id){
  
  if (base::missing(DataSet)) {
    return(cat("  find_colindex(BasicData, c('brand', 'satprice')) " ))  }
  
  tmp<-DataSet
  rm(DataSet)
  
  tmp_colnames<-colnames(tmp)
  n<-length(tmp_colnames)  # DataSet의 총변수 갯수
  index_n<-length(index_id)  # 연구자가 선택한 변수명들의 갯수
  index_n2<-index_n*2
  r=c()
  r2=c()
  
  for(j in 1:index_n){
    for(i in 1:n){
      if(index_id[j]==tmp_colnames[i]) {r=c(r,i, ',')}
    }  }
  
  r<-as.data.frame(r)
  r<-r[ 1:(index_n2-1), ]
  r<-as.vector(r)
  
  
  for(i in 1:index_n){
    r2=c(r2,index_id[i],',')
  }
  
  r2<-as.data.frame(r2)
  r2<-r2[ 1:(index_n2-1), ]
  r2<-as.vector(r2)
  
  cat('  Column Names: c(',r2,')','\n')
  cat('  Column Numbers: c(',r,')','\n')
  
}
