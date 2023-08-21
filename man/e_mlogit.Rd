\name{e_mlogit}
\alias{e_mlogit}
\title{Calc: multinomial logit estimation}
\usage{
e_mlogit()
}
\description{
Multinomial Logit model
}
\examples{

library(nnet); library(ztable)
결과<-multinom(gpa~involve+exhaust+gender+age, data=Adata)
options(ztable.type='viewer')
ztable(결과)

## 확률의 계산
predict(결과, newdata=data.frame(involve=3, exhaust=2, gender=1, age=25), type='prob')

## Multinomial Logit
library(nnet)   
Adata<-read.csv('Data34_mlogit3.csv', header=TRUE)
mlogit.결과 <- multinom(job ~ ed, data=Adata)

## 시뮬레이션 자료: c(상수항, 교육기간은 10년)
inputdata<-c(1, 10)   
mcoef<-coef(mlogit.결과)
y2<-sum(mcoef[1,1:2]*inputdata)   
y3<-sum(mcoef[2,1:2]*inputdata)   

}