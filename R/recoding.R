recoding<-function(name_dataset,  select_columns ) {

if (base::missing(name_dataset)) {
        cat("  df<-as.data.frame(df)", '\n')
        return( cat("  df<-recoding(df, 변수명 또는 컬럼번호) ", '\n') )
    }

##-----------------------------------
# 변수명을 컬럼 번호로 변경시키는 함수
##------------------------------------
find_col2<-function(DataSet, index_id ){
  tmp<-DataSet
  rm(DataSet)
  tmp_colnames<-colnames(tmp)
  n<-length(tmp_colnames)  # DataSet의 총변수 갯수
  
  for(i in 1:n){
    if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
  }
}

if(is.numeric(select_columns)==F) {select_columns<-find_col2(name_dataset, select_columns) }

##----------------------------------------


name_dataset<-as.data.frame(name_dataset)
tmpx<-name_dataset[ , select_columns]
k<-ncol(name_dataset)
n<-nrow(name_dataset)
origindata=c()
for(i in 1:n){
  origindata=c(origindata, tmpx[i])
}

tmp_unique<-sort(unique(origindata))
tmp_unique_count<-length(tmp_unique)
origindata2<-rep(NA, n)
cat( 'The variable is defined as :', class(origindata)  , '\n')
cat("values in the '", colnames(name_dataset)[select_columns], "' is ",'\n')
cat('----------------------', '\n')
print(tmp_unique)
cat('----------------------', '\n')
cat(' ', '\n')

if( class(origindata)=="character") {
  for(j_count in 1:tmp_unique_count){
    cat(tmp_unique[j_count])
    input_number<-readline("Change it to :")
    origindata2[origindata==tmp_unique[j_count]]<-input_number
  }  }


if( class(origindata)=="numeric" | class(origindata)=="integer" ) {
  for(j_count in 1:tmp_unique_count){
    cat(tmp_unique[j_count])
    input_number<-readline("Change it to :")
    origindata2[origindata==tmp_unique[j_count]]<-input_number
  }  }

  name_dataset2<-cbind(name_dataset, origindata2)
  colnames(name_dataset2)[k+1] <- paste0(colnames(name_dataset[select_columns]), "_se" , sep='')
  return(name_dataset2)
}

