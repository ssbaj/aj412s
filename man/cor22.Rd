\name{cor22}
\alias{cor22}
\title{Calc: correlation of chosen data set}
\usage{
cor22()
}
\description{
Calculating the correlation coefficient of data set.
}
\examples{

x<-c(2,4,5,NA,5,8,NA,9,13,22,Inf, -Inf, 34,66,69)
y<-c(4,5,7,3,7,1,4,17,22,4,5,23,25,43,66)
z<-c(1,1,1,1,2,2,2,3,3,3,4,4,4,4,4)
tmp<-cbind(x,y,z)
tmp<-as.data.frame(tmp)
rm(x,y,z)

cor22(tmp, method='pearson' or 'spearman' or 'kendall')


}