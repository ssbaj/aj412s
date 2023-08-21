\name{c2n}
\alias{c2n}
\title{Converting character into numbers}
\usage{
c2n()
}
\description{
Converting character into numbers
}
\examples{
df<-read.csv('Data20_Mroz.csv', header=T)
df$wc2<-c2n(df$wc)
df$hc2<-c2n(df$hc)
}