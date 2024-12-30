\name{mkdum22}
\alias{mkdum22}
\title{Making dummy variables}
\usage{
mkdum22()
}
\description{
Making dummy variables
}
\examples{

df<-mkdum22(data_set, satprice)

EXAMPLE: 

df<-BasicData
df<-mkdum22(df, brand) # making dummies for brand
df<-mkdum22(df, edu) # making dummies for edu

}