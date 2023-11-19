# Korean encoding command is not necessary
# It is a modification of read.csv
opencsv <- function(name_dataset, header=FALSE, skip=0) {
if (base::missing(name_dataset)) {
	cat("", '\n')
	cat("  # if there is Korean in your file, use opencsv() ", '\n')
    	cat("  # if there are variable names in the first line, header=T ", '\n')
       	cat("  # if 1st~8th lines are comments and you want to skip them, skip=8 ", '\n')
	cat("", '\n')
	cat("  df<-opencsv( 'KoreanFinance2018.csv', header=F, skip=0 ) ", '\n')
	return(cat("", '\n') )
	}

tmp.df<-read.csv(name_dataset, fileEncoding='CP949', encoding='UTF-8', header=header, skip=skip)
return(tmp.df) }
