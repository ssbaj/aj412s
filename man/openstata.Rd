\name{openstata}
\alias{openstata}
\title{Loading Stata file containing Korean}
\usage{
openstata()
}
\description{
Loading Stata file containing Korean
}

\examples{

  # file에 1번~8번행이 설명문일 때, 설명문을 제거한 후 데이터를 로딩하려면 skip=8  
 
  df<-openstata( '가계금융복지2018.dta', header=F, skip=0 )  
  
}
