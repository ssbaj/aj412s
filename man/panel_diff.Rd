\name{panel_diff}
\alias{panel_diff}
\title{panel_diff}
\usage{
panel_diff()
}
\description{
differencing panel dataset
}
\examples{

# 1. 예제 df를 만들기위한 벡터 생성 ---------  
  index <- c('A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10', 'A11', 'A12')  
  area <- c('A', 'A', 'A', 'B', 'B', 'B', 'C', 'C', 'C', 'D', 'D', 'D')  
  year <- c(2001, 2002, 2003, 2001, 2002, 2003, 2001, 2002, 2003, 2001, 2002, 2003)  
  변수1 <- c(10, 14, 18, 21, 23, 25, 32, 36, 40, 42, 44, 46)  
  변수2 <- c(13, 68, 86, 23, 78, 98, 34, 79, 92, 53, 83, 100)  
    
  # 2. 벡터들을 모아 데이터 프레임 생성  
  df <- data.frame(index, ind2, date2, 변수1, 변수2)  
    
  # plm패키지의 pdata.frame을 사용: 패널데이터 지정 -> 차분difference  
  # df 차분. 단, area, year, index, 문자변수는 차분 제외  -----------  
    
  library(plm)  
  panel_data <- pdata.frame(panel, index = c('area', 'year'))   
  diffDataSet<-panel_diff(panel_data)  

}
