mkNAs<-function(tmp_data, col_number, NAs_number){

if (base::missing(tmp_data)) {
    cat("  df<-as.data.frame(df) ", '\n')
    return(cat("  df <- mkNAs(df, 변수명, 88)  *NOTE: 88=변수의 레코드를 88개 랜덤하게 골라 NA로 치환시켜라 ") ) }

##-----------------------------------
# 변수명을 컬럼 번호로 변경시키는 함수
##------------------------------------
find_col2<-function(DataSet, index_id ){
  tmp_colnames<-colnames(DataSet)
  n<-length(tmp_colnames)  # DataSet의 총변수 갯수
  
  for(i in 1:n){
    if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
  }
}

##----------------------------------------
# find_col2()를 사용해 컬럼번호 찾기
if(is.numeric(col_number)==F) {col_number<-find_col2(tmp_data, col_number) }
##----------------------------------------


tmp_data<-as.matrix(tmp_data)
tmp_dataV2<-tmp_data
n<-nrow(tmp_dataV2)
selRows<-sample(1:n, NAs_number, replace=F)
tmp_dataV2[selRows, c(col_number)]<-NA
tmp_NAs <- tmp_dataV2[ ,c(col_number)]

tmp_data<-cbind(tmp_data, tmp_NAs)
tmp_data<-as.data.frame(tmp_data)

last_n<-ncol(tmp_data)
colnames(tmp_data)[last_n]<-paste0( colnames(tmp_data[col_number]) , 'NAs', sep='')

return(tmp_data)
}
