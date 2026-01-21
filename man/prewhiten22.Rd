\name{prewhiten22}
\alias{prewhiten22}
\title{Transfer function: prewhiten}
\usage{
prewhiten22()
}
\description{
prewhiten of y variable
}
\examples{

  TSA 패키지: Transfer function model을 위한 prewhiten 명령문  
  fit_x = arima(x, order = c(1, 0, 1))   
  prewhiten(x, y, x.model = fit_x) 

}