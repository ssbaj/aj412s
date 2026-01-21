\name{mkdate_series}
\alias{mkdate_series}
\title{making year-month date Dataset}
\usage{
mkdate_series()
}
\description{
making year-month date Dataset
}
\examples{

  Input date format: 
  start_y=2001, 
  start_m=4, 
  
  그리고 마지막 입력값이
  4를 입력하면 quarterly 데이터
  12를 입력하면 monthly 데이터

  # montly data start from 2015-04-01
  df<-mkdate_series(df, 2015, 4, 12)  
  
  # quarterly data start from 2015-04-02
  df<-mkdate_series(df, 2015, 4, 4)   
}