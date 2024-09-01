mkdate_form<-function(df, ymdata ){

if (base::missing(df)) {
	    cat("  df<-as.data.frame(df) ", '\n')
		cat("  Input: 2023Q4 or 2023.4 or2023-4 or 2023/4  ===>  Output: DATE_form=2023-04-01    ", '\n')
		return(cat("  df<-mkdate_form(df, 4=column of Year.Month ) ") )  }

options(warn = -1)
df<-as.data.frame(df)

change_ymdata1<-function(tmp_data){
tmpx1<-substring(tmp_data, 1, 4)
tmpx2<-substring(tmp_data, 6)
tmpx3<-paste0(tmpx1, "-0", tmpx2,"-01", sep="")
return(tmpx3)
}

change_ymdata2<-function(tmp_data){
tmpx1<-substring(tmp_data, 1, 4)
tmpx2<-substring(tmp_data, 5,6)
tmpx3<-paste0(tmpx1,"-" , tmpx2,"-01", sep="")
return(tmpx3)
}

df$DATE<-NA
df[ ,ymdata]<-as.character(df[ ,ymdata])

n<-nrow(df)

## 정교하게 코드를 작성하는 대신, 5번째 글자가 문자인지 숫자인지를 판단해 DATE를 만들자
tmp_mark <- substring( (df[ ,ymdata][1]) , 5, 5)

suppressWarnings({
	if( is.na(as.numeric(tmp_mark)) ) {tmp_mark2 <-0 } else {tmp_mark2 <-1 }
})

for (i in 1:n){
  if( (tmp_mark2 == 1 ) ) { df$DATE[i]<-change_ymdata2(df[,ymdata][i]) }
  else { df$DATE[i]<-change_ymdata1(df[,ymdata][i]) }
  }

df$DATE<-as.Date(df$DATE)
suppressPackageStartupMessages(library("dplyr"))
df <- df%>%relocate(DATE)

return(df)
}

