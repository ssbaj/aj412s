\name{icc}
\alias{icc}
\title{icc: Intraclass Correlation Coefficient}
\usage{
cor22()
}
\description{
Calculating the Intraclass Correlation Coefficient
}
\examples{

library(lme4); library(lmerTest)
Result <- lmer(y ~ ind1 + ind2 + (1|id), data=Adata, REML=F, subset=c(ind1>=0.03 & ind2 == 1))
icc(Result)

}