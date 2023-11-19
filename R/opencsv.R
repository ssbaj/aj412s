# Korean encoding command is not necessary
# It is a modification of read.csv
opencsv <- function(name_dataset, header=FALSE, skip=0) {
if (base::missing(name_dataset)) {
	cat("", '\n')
	cat("  # file에 한글이 포함된 csv파일 로딩 명령문 ", '\n')
    	cat("  # file에 변수명이 포함되었으면 header=T / 변수명이 없으면 따로 지정할 필요 없음 ", '\n')
       	cat("  # file에 1번~8번행이 설명문일 때, 설명문을 제거한 후 데이터를 로딩하려면 skip=8 ", '\n')
	cat("", '\n')
	cat("  df<-opencsv( '가계금융복지2018.csv', header=F, skip=0 ) ", '\n')
	return(cat("", '\n') )
	}

tmp.df<-read.csv(name_dataset, fileEncoding='CP949', encoding='UTF-8', header=header, skip=skip)
return(tmp.df) }
