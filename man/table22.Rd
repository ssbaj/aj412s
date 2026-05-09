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

 table22(desc22결과)
 table22(lm결과)
 table22(logit결과)

 library(sandwich); library(lmtest)
 RobustSE_OLS결과<-coeftest(X결과tmp, vcov = vcovHC(X결과tmp, type = 'HC1'))
 table22(RobustSE_OLS결과)

 table22(mlogit결과)
 엑셀파일은 Re년도월일시간초.xlsx파일로 저장됨

}
