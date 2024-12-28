openxlsx<-function(datasetname, header=T, skip=0, sheet=1) {

if (base::missing(datasetname)) {
	cat("# Loading Excel file ( .xlsx 또는 .xls) ", '\n')
	cat("  Adata<-openxlsx( KoreaData.xlsx ) ", '\n')
	cat("  옵션: Adata<-openxlsx( KoreaData.xlsx, header=T/F, skip=2 입니다 )", '\n')
	cat("  엑셀파일 첫 줄에 변수명이 없으면 자동으로 변수명V1, V2...이 추가됩니다 ", '\n')
	return(cat("  첫 줄에 변수명이 없을 때의 명령문: Adata<-openxlsx( KoreaData.xlsx, header=F)", '\n') )
	}

  if (!require(readxl)) {
    cat('Installing readxl package load Excel data .....', '\n')
    install.packages("readxl")
  }
  
  suppressMessages(library(readxl))
  
  datasetname <- deparse(substitute(datasetname))

  if(datasetname == "file.choose()") {datasetname <- file.choose() }

  tmp<-read_excel(datasetname, skip=skip, col_names=header, sheet=sheet, .name_repair = "minimal")
  num_vars <- ncol(tmp)

  variable_names <- paste0("V", 1:num_vars, sep = "")

  if(header==F) {
    for(i in 1:num_vars){
      colnames(tmp)[i] <- variable_names[i]
    } }

  return(tmp)
}

