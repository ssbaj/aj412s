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

  Input date format: start_y=2001, start_m=4, 1=monthly or 2=quarterly data

  df<-mkdate_series(df, 2001, 4, 1)   # montly data start from 2001-04-01
  df<-mkdate_series(df, 2021, 12, 1)  # montly data start from 2021-12-01
  df<-mkdate_series(df, 2015, 4, 2)   # quarterly data start from 2015-04-02
}