## How to use: 
## make 34 NAs in 4th column
## Adata <- mkNAs(Adata, 4, 34)

mkNAs<-function(tmp_data, col_number, NAs_number){

if (base::missing(tmp_data)) {
    cat("  df<-as.data.frame(df) ", '\n')
    return(cat("  df <- mkNAs(df, 9, 13)  *NOTE: 9=9번째 변수, 13=9번 변수의 레코드를 랜덤하게 13개 골라서 NA로 치환시켜라   ") ) }

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
