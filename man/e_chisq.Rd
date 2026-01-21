\name{e_chisq}
\alias{e_chisq}
\title{Calc: Chisquare test}
\usage{
e_chisq()
}
\description{
Calculating Chisquare test
}
\examples{
library(gmodels) 
Adata<-read.csv('Data30_Chisq.csv')  
CrossTable(Adata$gender, Adata$smoking, chisq=TRUE)   
셀자료수  
카이자승값  
셀자료수/가로합 
셀자료수/세로합 
셀자료수/총자료수 
}