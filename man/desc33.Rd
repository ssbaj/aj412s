\name{desc33}
\alias{desc33}
\title{Function of calculating mean,SD, and Quantile for all variables}
\usage{
desc33()
}
\description{
Function of calculating mean, SD, and Quantile for all variables
}
\examples{

 library(aj412s)
 df<-BasicData
 
 # default of digtis=3
 desc33(df)
 
 # Change digtis=5
 desc33(df, 5)
 
}