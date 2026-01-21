\name{mklog}
\alias{mklog}
\title{mklog}
\usage{
mklog()
}
\description{
Making log dataset
}
\examples{

## make log data for one varialbe
tmp<-mklog(dataset, 변수명)

EXAMPLE:
library(aj412s)
df<-BasicData

df<-mklog(df, satprice, conv)

1. 셀의 값<0 : NA
2. 변수의 셀이 -1보다 작으면 양수로 변경해 로그 변환 후, 
   음(-)의 부호를 붙인다.
}
