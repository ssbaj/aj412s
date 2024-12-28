\name{probit}
\alias{probit}
\title{probit}
\usage{
probit()
}
\description{
Instead of glm we use probit command
}
\examples{

# Adata에서 브랜드명이 GM인 경우, 가격만족도 추출하기

re<-probit(debt2~age+gender, data=df, subset=(age>20))


}
