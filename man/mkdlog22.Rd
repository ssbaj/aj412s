\name{mkdlog22}
\alias{mkdlog22}
\title{Make differenced log-variable and save it in the data frame}
\usage{
mkdlog22()
}
\description{
Make differenced log-variable and save it in the data frame
}
\examples{

# univariate variable에 로그를 취한 후, 1차 차분을 하는 명령문

df<-mkdlog22(df, satprice, conv, year)  ## 2=the number of columns for differencing

}