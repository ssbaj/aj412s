\name{Return_variablenames}
\alias{Return_variablenames}
\title{Return variablenames}
\usage{
Return_variablenames()
}
\description{
Return variablenames
}
\examples{

  LogitResult<-glm(dep~ind, family='binomial', data=df)
  tmp_vnames<-Return_variablenames(LogitResult)
  
  # tmp_vnames에 변수명이 기록
  # 첫번째 변수명: tmp_vanmes[1], 두번째 변수명: tmp_vanmes[2] ...

}