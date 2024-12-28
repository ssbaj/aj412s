\name{e_ttest1}
\alias{e_ttest1}
\title{Calc: t-test of the group mean}
\usage{
e_ttest1()
}
\description{
Calculating t-test of group mean
}
\examples{

library(jmv); library(readxl)  
Adata<-read_excel('foot.xlsx')  

jmv::ttestOneS(   
   data = df, # 내장데이터BasicData를 df로 저장   
   vars = expend,   
   testValue = 300,   
   desc = TRUE )   

}