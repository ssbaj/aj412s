openxlsx<-function(datasetname, header=F, skip=0) {

if (base::missing(datasetname)) {
	cat("", '\n')
	cat("  # if there are variable names in the first line, header=T ", '\n')
    cat("  # if 1st~8th lines are comments and you want to skip them, skip=8 ", '\n')
	cat("", '\n')
	cat("  df<-openxlsx( 'DATA2.xlsx', header=T, skip=0 ) ", '\n')
	return(cat("", '\n') )
	}

tryCatch({
tmp.df<-read_excel(datasetname, skip=skip, col_names=header)
}, error = function(e) {
  cat("  # Error: How to solve? # library(readxl); library(aj412s); openxlsx() ", '\n')
})
}

