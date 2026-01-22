\name{chk22}
\alias{chk22}
\title{Finding non-numeric cells}
\usage{
check_non_numeric()
}
\description{
Finding non-numeric cells
}
\examples{

  # 데이터셋 셀을 전부 조사해서 숫자가 아닌 셀을 찾아낸다
    chk22(데이터셋) OR  chk22(데이터셋$변수)

  # Non numeric cell을 NA로 변경하는 방법 ##  
  library(dplyr)  
  df <- df %>% mutate(across(c(변수1, 변수2, 변수3), ~ suppressWarnings(as.numeric(as.character(.)))))  

}