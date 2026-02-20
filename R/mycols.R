mycols<-function(a, b, c, my_col_number=10){

    if (base::missing(a)) {
    cat("  \033[1;31m  # ---------------------------------- ", '\n')
    cat("  \033[1;31m  mycols(Red비율, Green비율, Blue비율, 컬럼갯수) ", '\n')
    cat("  \033[1;31m  mycols(0.1, 0.2, 0.6, 컬럼갯수) ", '\n')
    cat("  \033[1;31m  # ---------------------------------- ", '\n')
    return( cat("   ", '\n') )  }

     mycols <- rgb(a, b, c, seq(0,1,length=my_col_number) )
     barplot(rep(1, my_col_number), space=0, axes=F, col=mycols)
     return(mycols)
 }

