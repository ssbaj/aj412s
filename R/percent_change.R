# 퍼센트로 바꾸는 명령문
# percent_change() 함수 정의
percent_change <- function(x) {

if (base::missing(x)) {
    cat("  To maker % , multiply the result by 100.  ", '\n')
    return(cat("  df$pch <- percent_change(df$kospi) "))  }
  
  c(NA, ( (x[-1]/x[-length(x)] ) -1) ) }
