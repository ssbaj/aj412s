# Loading SPSS file -- sav files

openspss<-function(datasetname, skip=0, header=T) {

if (base::missing(datasetname)) {
	cat("# Loading SPSS data file ( .sav ) ----", '\n')
	cat("  Adata<-openspss( KoreaData.sav ) ", '\n')
	cat("  옵션: Adata<-openspss( KoreaData.sav )", '\n')
	return(cat(" ", '\n') )
	}

    if (!require(haven)) {
    install.packages("haven")
  }

library(haven)
 
datasetname <- deparse(substitute(datasetname))

if(datasetname == "file.choose()") {datasetname <- file.choose() }


tmp.data<-read_sav(datasetname, skip=skip)

return(tmp.data)

}

