\name{mkdiff22}
\alias{mkdiff22}
\title{Make differenced variable and save it in the data frame}
\usage{
mkdiff22()
}
\description{
Make differenced variable and save it in the data frame
}
\examples{

# univariable variable을 1차 차분하는 명령문
df<-mkdiff22(df, year, satprice, invest)  # 2= the number of column for differencing

}