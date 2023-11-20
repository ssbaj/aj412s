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
  cat("  # Error: How to solve? ==>  library(readxl) ; df<-openxlsx(...)  ", '\n')
})


tmp<-read_excel(datasetname, skip=skip, col_names=header)
num_vars <- ncol(tmp)
variable_names <- paste0("V", 1:num_vars, sep = "")

if(header==F) {
for(i in 1:num_vars){
colnames(tmp)[i] <- variable_names[i]
} }

return(tmp)
}

