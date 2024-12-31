\name{desc22}
\alias{desc22}
\title{Function of calculating mean,SD, and Quantile for all variables}
\usage{
desc22()
}
\description{
Function of calculating mean, SD, and Quantile for all variables
}
\examples{

 library(aj412s)
 df<-BasicData
 
 # default of digtis=3
 desc22(df)
 
 # Change digtis=5
 desc22(df, 5)
 
}