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

df<-mkdum(data_set, nth variable)

EXAMPLE: 

df<-BasicData
df<-mkdum(df, 2) # making dummies for brand
df<-mkdum(df, 7) # making dummies for edu

  
}