recoding<-function(name_dataset,  select_columns ) {

if (base::missing(name_dataset)) {
        cat("\033[1;32m# 명령문 예제 -------- \033[0m ", '\n')
		cat("\033[1;32m  df<-as.data.frame(df) \033[0m ", '\n')
		cat("\033[1;32m  미싱데이토로 표시한다면 NA 입력 \033[0m ", '\n')
        return( cat("\033[1;32m  df<-recoding(df, 변수명) \033[0m ", '\n') )
    }

cat("\033[1;31m## 문자 레코드를 숫자로 바꾸기 --------\033[0m ", '\n')

name_dataset<-as.data.frame(name_dataset)

##-----------------------------------
# 변수명을 컬럼 번호로 변경시키는 함수
##------------------------------------
find_col2<-function(DataSet, index_id ){
tmp_colnames<-colnames(DataSet)
n<-length(tmp_colnames)
for(i in 1:n){
  if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
}
}

var_name <- deparse(substitute(select_columns))

select_columns<-find_col2(name_dataset, var_name)
##----------------------------------------


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
  
  suppressWarnings( ifelse( !is.na(unique(as.numeric(name_dataset2[,k+1] ))),  ( name_dataset2[ ,k+1]<-as.numeric(name_dataset2[,k+1]) ) , (name_dataset2[,k+1]) ) )

  return(name_dataset2)
}

