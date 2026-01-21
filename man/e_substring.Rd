\name{e_substring}
\alias{e_substring}
\title{How to use : e_substring}
\usage{
e_substring()
}
\description{
How to use : e_substring
}
\examples{

## string문자열 예제 명령문 ------   
    
  v1<-c(1,2,3)  
  v2<-c('경기도 일산구', '경기도 수지구', '경기도 성남시')  
  df<-data.frame(v1, v2)  
    
## df 데이터셋 ------   
  > head(df)  
   v1            v2  
  1 경기도 일산구  
  2 경기도 수지구  
  3 경기도 성남시  
    
  for(i in 1:3) {  
# nchar  
  n<-nchar(df$v2[i])  
# substring  
  print(substring(df$v2[i], n))  
# gsub  
  Ex2<-gsub('경기도', 'GG', df$v2[i])  
  print(Ex2)  
}
}