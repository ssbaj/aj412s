openxlsx<-function(datasetname, header=T, skip=0, sheet=1) {

  if (base::missing(datasetname)) {
    cat("", '\n')
    cat("  # if there are variable names in the first line, header=T ", '\n')
    cat("  # if 1st~8th lines are comments and you want to skip them, skip=8 ", '\n')
    cat("", '\n')
    cat("  df<-openxlsx( 'DATA2.xlsx', header=T, skip=0 ) ", '\n')
    return(cat("", '\n') )
  }


  if (!require(readxl)) {
    install.packages("readxl")
  }

  library(readxl)
  tmp<-read_excel(datasetname, skip=skip, col_names=header, sheet=sheet, .name_repair = "minimal")
  num_vars <- ncol(tmp)

  variable_names <- paste0("V", 1:num_vars, sep = "")

  if(header==F) {
    for(i in 1:num_vars){
      colnames(tmp)[i] <- variable_names[i]
    } }

  return(tmp)
}

