scatmat<-function(... , nclass=NULL) {

if (base::missing(...)) {
	cat("  # NA미싱데이터가 있으면 실행불가능 ------------------------------- ", '\n')
	cat("  # tmp<-Adata[, c('conv','satprice','age','edu','nfamily')] ", '\n')
	cat("  # tmp<-tmp[complete.cases(tmp), ]  # NA미싱데이터가 있으면 실행불가능", '\n')
	return( cat("  scatmat(tmp) ", '\n') )
}

cat("  # ---------------------------------------------------------------------------- ", '\n')
cat("  # complete.cases명령문을 사용해 NA미싱데이터를 제거한 후, scatmat을 실행합니다 ", '\n')
cat("  tmp<-Adata[, c('conv','satprice','age','edu','nfamily')] ", '\n')
cat("  tmp<-tmp[complete.cases(tmp), ]  # NA미싱데이터가 있으면 실행불가능", '\n')
cat("  scatmat(tmp) ", '\n')
cat("  viewer의 그래프를 보면, 대각선 변수가 x축 변수, 교차되는 변수가 y축 변수 ", '\n')
cat("  # ---------------------------------------------------------------------------- ", '\n')

  pairs( cbind(...) ,
         panel=function(x,y){
           points(x,y)
           abline(lm(y~x), lty=2)
           lines(lowess(x,y))
         },
         diag.panel=function(x){
           par(new=T)
           hist(x, main="", axes=F, nclass=nclass)
         } 
      )
  }


