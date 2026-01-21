\name{scatmat}
\alias{scatmat}
\title{Showing Density and lowess Relationship}
\usage{
scatmat()
}
\description{
Showing Density and lowess Relationship
}
\examples{
library(aj412s)
Adata<-KoreaMacro
scatmat(Adata$환율, Adata$cd91, Adata$gdp)
}