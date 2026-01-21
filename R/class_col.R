class_col <-function(data) {

if (base::missing(data)) {
cat(" \033[1;36m# Examples ---------- \033[0m", '\n' )
return( cat(" \033[1;36m  class_col(데이터셋) \033[0m", '\n' ) ) }

df<-as.data.frame(data) 
rm(data)
  cat(" ", '\n')
  cat("\033[1;31m # class of variables -------------------\033[0m ", '\n')
  mycolnames<-colnames(df)
  colnumber<-length( colnames(df))

  for(i in 1:colnumber) {
    tmp<-class(df[,i])
	
	if(tmp=='numeric') {
	     cat( "   ", mycolnames[i],'(',i,') ', sep="")
	     cat("\033[1;41m numeric  \033[0m ", sep="")
		 cat("\n")  }

    else if(tmp=='character') {
	     cat( "   ", mycolnames[i],'(',i,') ', sep="")
	     cat("\033[1;34m charcter  \033[0m ", sep="")  
		 cat("\n")  }
    else if(tmp=='factor') {
	     cat( "   ", mycolnames[i],'(',i,') ', sep="")
	     cat("\033[1;35m factor  \033[0m ", sep="")  
		 cat("\n")  }
	else { cat( "   ", mycolnames[i],'(',i,') ' , tmp , sep="", '\n' )  }

	  }
  
  cat("\033[1;31m # --------------------------------------\033[0m ", '\n')
  cat(" ", '\n')

  }
