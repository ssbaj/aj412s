scatmat<-function(..., nclass=NULL) {

if (base::missing(...)) {
	cat("  tmp<-Adata[, c('conv','satprice','age','edu','nfamily')] ", '\n')
	cat("  tmp<-tmp[complete.cases(tmp), ]  # NA미싱데이터가 있으면 실행불가능", '\n')
	return( cat("  scatmat(tmp) ", '\n') )
}

  pairs( cbind(...),
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


