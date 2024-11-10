# 퍼센트로 바꾸는 명령문
# percent_change() 함수 정의
percent_change <- function(x_trend) {

if (base::missing(x_trend)) {
    cat("  To maker % , multiply the result by 100.  ", '\n')
    return(cat("  df$pch <- percent_change(df$kospi) "))  }
  
  x_trend <- (x_trend - dplyr::lag(x_trend) ) /dplyr::lag(x_trend)
  
  return(x_trend) }
