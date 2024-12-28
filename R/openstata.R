# Loading Stata file -- dta files

openstata<-function(datasetname, skip=0, header=T) {

if (base::missing(datasetname)) {
	cat("# Loading Stata data file( .dta ) ---- ", '\n')
	cat("  Adata<-openstata( MROZ.dta ) ", '\n')
	cat("  옵션: Adata<-openstata( MROZ.dta )", '\n')
	return(cat(" ", '\n') )
  }

if (!require(haven)) {
    install.packages("haven")
  }

datasetname <- deparse(substitute(datasetname))
if(datasetname == "file.choose()") {datasetname <- file.choose() }

library(haven)
tmpdata <-read_dta(datasetname, skip=skip)
return(tmpdata)
}
