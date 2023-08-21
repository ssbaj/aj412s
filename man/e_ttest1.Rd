\name{e_ttest1}
\alias{e_ttest1}
\title{Calc: t-test value}
\usage{
e_ttest1()
}
\description{
Calculating t-test value.
}
\examples{

library(jmv); library(readxl)  
Adata<-read.csv('Data1_CrispyRoll.csv')  
jmv::ttestOneS(   
data = Adata, # 데이터프레임명을 Adata로 지정   
vars = weight,   
testValue = 10,   
# norm = TRUE,  # normality 검정/Shapiro-Wilk검정   
desc = TRUE )   
}