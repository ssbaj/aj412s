class_col <-function(df) {
  cat(" ", '\n')
  cat("\033[1;31m  # class of variables -------------------\033[0m ", '\n')
  cat("\033[1;32m  # variable name(column number)  \033[0m ", '\n')
  mycolnames<-colnames(df)
  colnumber<-length( colnames(df))
  for(i in 1:colnumber) {
    cat( "  ", mycolnames[i],'(',i,') ' , class( df[1, i] ) , sep="", '\n' )  }
  cat("\033[1;31m  # --------------------------------------\033[0m ", '\n')
  cat(" ", '\n')
  }
