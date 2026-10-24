\name{mktable}
\alias{mktable}
\title{Making Table}
\usage{
mktable()
}
\description{
Making Table
}
\examples{

# Examples ----------  
 library(aj412s)
 tmp<-desc22(BasicData)
 mktable(tmp)

 re<-lm(expend~age+gender,data=df)
 mktable(summary(re))
 
}
