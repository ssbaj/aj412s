\name{find_outlier}
\alias{find_outlier}
\title{find_outlier}
\usage{
find_outlier()
}
\description{
find Outliers
}
\examples{

# 회귀분석결과에서 outlier인 레코드를 찾는다

  df<-BasicData
  회귀분석결과<-lm(debt~inc, data=df)
  find_outlier(회귀분석결과)
    또는,
  TempData<-find_outlier(회귀분석결과)

}
