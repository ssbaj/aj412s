\name{table22}
\alias{table22}
\title{Making excel file for recording results}
\usage{
table22()
}
\description{
Making excel file for recording results
}
\examples{

 예를 들어
 library(sandwich); library(lmtest)
 re<-lm(satprice~conv+age+gender,data=df)
 summary(re)
 coeftest(re, vcov = vcovHC(re, type = 'HC1'))
 
 화면에 나타나는 분석결과를 
 분석결과를 임의의 text(예 mytest.txt)파일에 저장한다

 table22()
 
 화면이 나타나면 mytest.txt파일을 선택하면 결과는
 엑셀파일로 저장된다

}
