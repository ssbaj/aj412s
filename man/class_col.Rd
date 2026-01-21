\name{class_col}
\alias{class_col}
\title{Presenting the characteristics of variables}
\usage{
class_col()
}
\description{
Present the characteristics of variables
}
\examples{

library(aj412s)
df<-opencsv('Data20_Mroz.csv', header=T)
class_col(df)

}