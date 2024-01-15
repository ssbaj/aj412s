\name{cor33}
\alias{cor33}
\title{Calc: correlation only selected data}
\usage{
cor33()
}
\description{
Calculating the correlation coefficient.
}
\examples{

x<-c(2,4,5,NA,5,8,NA,9,13,22,Inf, -Inf, 34,66,69)
y<-c(4,5,7,3,7,1,4,17,22,4,5,23,25,43,66)
z<-c(1,1,1,1,2,2,2,3,3,3,4,4,4,4,4)
data<-cbind(x,y,z)
data<-as.data.frame(data)
rm(x,y,z)

cor33(data$x, data$y, data$z, 1)
cor33(data$x, data$y, data$z, 2)
cor33(data$x, data$y, data$z, 3)
cor33(data$x, data$y, data$z, 4)


}