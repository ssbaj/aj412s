opencsv <- function(name_dataset, header=TRUE, skip=0) {

if (base::missing(name_dataset)) {
	cat("# Loading csv file ( .csv ) ----", '\n')
	cat("  df<-opencsv( data.csv ) ", '\n')
	cat("  옵션: df<-opencsv( data.csv, header=T 또는 F, skip=1 등 입니다 )", '\n')
	return(cat("  한글보호: write.csv(data, 'Bdata.csv', fileEncoding='CP949') ", '\n') )
	}

name_dataset <- deparse(substitute(name_dataset))

if(name_dataset == "file.choose()") {name_dataset <- file.choose() }

name_dataset[name_dataset==""]<-NA

tmp.df<-read.csv(name_dataset, fileEncoding='CP949', encoding='UTF-8', header=header, skip=skip)

return(tmp.df) }
