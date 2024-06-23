scatmat<-function(..., nclass=NULL) {

if (base::missing(...)) {
    cat("  df<-as.data.frame(df with only numbers)", '\n')
	return( cat("  scatmat(df) ", '\n') )
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


