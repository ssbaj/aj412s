\name{desc2}
\alias{desc2}
\title{Function of calculating mean,SD, and Quantile for all variables}
\usage{
desc2()
}
\description{
Function of calculating mean, SD, and Quantile for all variables
}
\examples{

 library(aj412s)
 df<-BasicData
 
 # default of digtis=3
 desc2(df)
 
 # Change digtis=5
 desc2(df, 5)
 
}