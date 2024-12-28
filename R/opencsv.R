# Korean encoding command is not necessary
# It is a modification of read.csv
opencsv <- function(name_dataset, header=TRUE, skip=0) {

if (base::missing(name_dataset)) {
	cat("# Loading csv file ( .csv ) ----", '\n')
	cat("  Adata<-opencsv( KoreanFinance2018.csv ) ", '\n')
	return(cat("  옵션: Adata<-opencsv( KoreanFinance2018.csv, header=T/F, skip=2 입니다 )", '\n') )
	}

name_dataset <- deparse(substitute(name_dataset))

if(name_dataset == "file.choose()") {name_dataset <- file.choose() }

name_dataset[name_dataset==""]<-NA

tmp.df<-read.csv(name_dataset, fileEncoding='CP949', encoding='UTF-8', header=header, skip=skip)

return(tmp.df) }
