\name{mkdum}
\alias{mkdum}
\title{Making dummy variables}
\usage{
mkdum()
}
\description{
Making dummy variables
}
\examples{

df<-mkdum(data_set, satprice)

EXAMPLE: 

df<-BasicData
df<-mkdum(df, brand) # making dummies for brand
df<-mkdum(df, edu) # making dummies for edu

  
}