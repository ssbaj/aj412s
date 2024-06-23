scatmat<-function(..., nclass=NULL) {

if (base::missing(...)) {
	cat("  tmp<-Adata[, c('conv','satprice','age','edu','nfamily')] ", '\n')
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


