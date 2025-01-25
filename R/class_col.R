class_col <-function(df) {
  cat(" ", '\n')
  cat("\033[1;31m # class of variables -------------------\033[0m ", '\n')
  mycolnames<-colnames(df)
  colnumber<-length( colnames(df))

  for(i in 1:colnumber) {
    tmp<-class(df[,i])
	
	if(tmp=='numeric') {
	     cat( "   ", mycolnames[i],'(',i,') ', sep="")
	     cat("\033[1;32m numeric  \033[0m ", sep="")  
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
