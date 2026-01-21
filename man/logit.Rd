\name{logit}
\alias{logit}
\title{logit}
\usage{
logit()
}
\description{
Instead of glm we use logit command
}
\examples{

# Adata에서 브랜드명이 GM인 경우, 가격만족도 추출하기

re<-logit(debt2~age+gender, data=df, subset=c(age>20))


}
