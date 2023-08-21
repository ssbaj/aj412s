\name{e_ftest1}
\alias{e_ftest1}
\title{Calc: One-Way Ftest}
\usage{
e_ftest1()
}
\description{
Calculating One-Way Ftest
}
\examples{
library(jmv); library(readxl) 
Adata<-read_excel('Data27_Royalty_F검정.xlsx')  
jmv::anovaOneW(      
  formula = royalty ~ brand,  
  data = Adata,  
  welchs = FALSE,  # Levene’s Test결과 이분산이면 welchs=TRUE, fishers=FALSE  
  fishers = TRUE,  # Levene’s Test결과 등분산이면 welchs=FALSE, fishers=TRUE    
  desc = TRUE,    
  eqv = TRUE,  # Levene’s Test를 실행    
 #phMethod = 'gamesHowell')  # Levene’s Test결과 이분산이면 gamesHowell검정 실행    
  phMethod = 'tukey')  # Levene’s Test결과 등분산이면 tukey검정 실행
}