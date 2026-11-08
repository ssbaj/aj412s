checkresid<-function(ols_result){

if (base::missing(ols_result)) {
      cat("   ", "\n")
      cat("  \033[1;31m# Original 명령문 ----- \033[0m", "\n")
      cat("  \033[1;32m  plot( ols_result, which=c(1:5) ) \033[0m", "\n")
      cat("  \033[1;32m \033[0m", "\n")
      cat("  \033[1;31m# checkresid 명령문 ----- \033[0m", "\n")
      cat("  \033[1;31m# checkresid(ols_result) ----- \033[0m", "\n")
      return(cat("   \n"))
    }


plot( ols_result, which=c(1:5) )

}