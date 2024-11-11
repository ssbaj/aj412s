# 퍼센트로 바꾸는 명령문
# percent_change() 함수 정의
percent_change <- function(x_trend) {

if (base::missing(x_trend)) {
    cat("  To make % value, multiply the result by 100.  ", '\n')
    return(cat("  df$pch <- percent_change(df$kospi) "))  }

if (!require(dplyr)) {
    cat('Automatically Installing dplyr package because','\n')
	cat('dplyr is necessary for this function','\n')
	cat('If an error occurs, connect to the network','\n')
	install.packages("dplyr")
  }

suppressPackageStartupMessages(library("dplyr"))

x_trend <- (x_trend - dplyr::lag(x_trend) ) /dplyr::lag(x_trend)
  
return(x_trend) }
