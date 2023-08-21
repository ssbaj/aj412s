\name{e_moonBook}
\alias{e_moonBook}
\title{Making Tables}
\usage{
e_moonBook()
}
\description{
Making Tables
}
\examples{
  library(moonBook); library(ztable); library(readxl); library(aj412s)
  options(ztable.type='viewer')

  ## 내장 데이터 사용
  df<-BasicData

  ## 회귀분석/로짓분석 결과를 테이블로 만드는 방법
  re <- lm(car_satprice~gender+car_conv, data=df)
  ztable(re) # Rstudio의 viewer에서 복사 -> 엑셀 -> 한글/워드에 복사

  ## 연속형 데이터에 대한 기술

  ## 범주형 데이터에 대한 기술
  re<-mytable(gender~car_satprice, data=df)  # 복사해서 텍스트로 저장 - 엑셀 - 한글/워드에 복사
  ztable(re)

  # max.ylev는 더미변수가 최대 몇개의 숫자로 코딩되었는지 지정
  # 독립변수 코딩값이 max.ylev값을 넘어서면 독립변수를 연속형변수로 인식, max.ylev의 디폴트=5

  # 통계분석결과의 출력
  re<-mytable(~gender+age+edu, data=df, max.ylev=7)
  ztable(re)

}