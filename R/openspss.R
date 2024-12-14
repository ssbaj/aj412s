# Loading SPSS file -- sav files

openspss<-function(datasetname, skip=0, header=T) {

  if (base::missing(datasetname)) {
    cat("", '\n')
    cat("  # It does not need header option. It loads SPSS dataset files.", '\n')
    cat("  # If 1st~8th lines are comments and you want to skip them, skip=8. ", '\n')
    cat("    df<-opensav('DATA2.sav') OR, df<-opensav( 'DATA2.sav', skip=8 ) ", '\n')
    cat(" ", '\n')
    cat("  # How to make variable labels ------------------------", '\n')
    cat("    library(labelled) ", '\n')
    cat("    var_label(df$studyno1) <- 'counting number of ID' ", '\n')
    cat("    var_label(df$studyno1) ", '\n')
    return(cat("", '\n') )
  }

    if (!require(haven)) {
    install.packages("haven")
  }

library(haven)
tmp<-read_sav(datasetname, skip=skip)

  return(tmp)
}

