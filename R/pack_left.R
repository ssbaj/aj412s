pack_left <- function(Bdata){

if (base::missing(Bdata)) {
    cat("  원본 data: 창업기업실태조사 -- 창업이유(V18~V27)  ", "\n")
	cat("  Bdata<-Adata[,c(18:27)]  ", "\n")
	cat("  tmp<-pack_left(Bdata)  ", "\n")
	cat("  tmp데이터셋에서 check변수 지우기  ", "\n")
	cat("  원본자료인 Adata(V1~V166) 및 tmp(index=V167, V168~V170) 병합하기  ", "\n")
	cat("  Cdata<-merge(Adata, tmp, by='index')  ", "\n")
	cat("  tmp2<-pack_down(Cdata, 170, 168)  #폐업원인3 -> 폐업원인1 레코드로 복사 ", "\n")
	return(cat("  tmp3<-pack_down(tmp2, 169, 168)   #폐업원인2 -> 폐업원인1 레코드로 복사 "))  }


ncol_number<-ncol(Bdata)
Bdata$index<-NA
Bdata$index<-as.numeric(rownames(Bdata))
Bdata$check<-NA
Bdata$check<-rowSums(Bdata[,c(1:ncol_number)], na.rm=T)
Bdata<-Bdata[ Bdata$check>0,  ]

for(k in 1:(ncol_number-1)){
for(j in 1:nrow(Bdata)){
for(i in 1:(ncol_number-1))  {
if( is.na(Bdata[ j, i])   ) {
Bdata[ j, i] <- Bdata[ j, i+1]
Bdata[ j, i+1] <- NA }
}}}
return(Bdata)
cat('NA만 있는 레코드는 삭제. index는 원본 데이터셋의 인덱스. check변수는 지우세요.', '\n')
}
