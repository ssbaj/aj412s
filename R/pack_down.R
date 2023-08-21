pack_down <-function(Adata, FromCol, ToCol){

if (base::missing(Adata)) {
    cat("  원본 data: 창업기업실태조사 -- 창업이유(V18~V27)  ", "\n")
	cat("  Bdata<-Adata[,c(18:27)]  ", "\n")
	cat("  tmp<-pack_left(Bdata)  ", "\n")
	cat("  tmp데이터셋에서 check변수 지우기  ", "\n")
	cat("  원본자료인 Adata(V1~V166) 및 tmp(index=V167, V168~V170) 병합하기  ", "\n")
	cat("  Cdata<-merge(Adata, tmp, by='index')  ", "\n")
	cat("  tmp2<-pack_down(Cdata, 170, 168)  #폐업원인3 -> 폐업원인1 레코드로 복사 ", "\n")
	return(cat("  tmp3<-pack_down(tmp2, 169, 168)   #폐업원인2 -> 폐업원인1 레코드로 복사 "))  }


LastNumber <- nrow(Adata)
for(i in 1:LastNumber){
	if(!is.na(Adata[i, FromCol]))  {
		Adata[(LastNumber+1), ] <-Adata[i, ]
		Adata[LastNumber+1, ToCol]<-Adata[i, FromCol]
		Adata[i, FromCol] <- NA
		Adata[LastNumber+1, FromCol]<-NA
		LastNumber<-LastNumber+1
		}
} 
return(Adata)
}
