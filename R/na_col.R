na_col <- function(dataset) {
  
  # 1. 데이터셋이 입력되지 않았을 때 도움말 출력
  if (base::missing(dataset)) {
    cat("  # 각 변수에 NA미싱데이터의 갯수를 체크 ------ ", '\n')
    return(cat("    na_col(df) \n"))
  }
  
  df <- data.frame(dataset)
  n <- ncol(df)
  
  # 2. 각 컬럼(변수)별로 반복
  for(i in 1:n) {
    
    # 현재 컬럼의 NA 개수 계산
    tmp.no <- sum(is.na(df[, i]))
    
    # NA가 1개 이상인 경우
    if(tmp.no > 0) {
      # 변수명 : 개수 *** 출력
      cat(colnames(df)[i], ' : ', tmp.no)
      cat("\033[1;31m *** \033[0m", '\n')
      
      # [수정된 부분] NA가 있는 행(Row) 번호 추출 및 출력
      na_rows <- which(is.na(df[, i])) 
      cat("    \033[1;33mNA Row:\033[0m", na_rows, "\n")
      
    } else {
      # NA가 없는 경우: 변수명 : 0 출력
      cat(colnames(df)[i], ': ', tmp.no, '\n') 
    }
  }
}