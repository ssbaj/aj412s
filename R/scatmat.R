scatmat<-function(..., nclass=NULL) {
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


