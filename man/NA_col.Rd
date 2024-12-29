\name{na_col}
\alias{na_col}
\title{Function of calculating mean,SD, and Quantile for all variables}
\usage{
na_col()
}
\description{
Function of calculating mean, SD, and Quantile for all variables
}
\examples{

 library(aj412s)
 df<-BasicData
 
 na_col(df)  # 각 변수별로 NA 갯수를 알려줌
 
}