\name{mkdate}
\alias{mkdate}
\title{making year-month date Dataset}
\usage{
mkdate()
}
\description{
making year-month date Dataset
}
\examples{

   mkdate_series(df, start_y, type=y/m/q, start_p=시작월/분기)
    - type=y: 연 단위 (12월 31일 기준)
    - type=m: 월 단위 (start_p에 시작 월 입력, 기본값 1)
    - type=q: 분기 단위 (start_p에 시작 분기 입력 1~4, 기본값 1)

}