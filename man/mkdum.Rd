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

df<-mkdum(data_set, 'satprice' OR nth variable)

EXAMPLE: 

df<-BasicData
df<-mkdum(df, 'brand' OR 2) # making dummies for brand
df<-mkdum(df, 'edu' OR 7) # making dummies for edu

  
}