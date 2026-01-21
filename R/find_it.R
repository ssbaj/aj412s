find_it <- function(data, value, column_name) {
  # 주어진 변수명에 해당하는 컬럼의 값을 찾고, 해당 값이 있는 행의 인덱스를 반환
  
  if (base::missing(data)) {
     cat("\033[1;32m  # 명령문: find_it(데이터셋, 변수값, 변수명) \033[0m ", '\n')
     cat("\033[1;32m  # 명령문: 변수값이 문자이면 따옴표로 감싸고, 숫자이면 그대로 입력 \033[0m ", '\n')
     cat("  find_it(df, 'David', Name)",'\n')
     cat("  find_it(df, 36, Age)",'\n')
     return(cat("  find_it(df, NA, ID)",'\n'))  }


  column_name <- deparse(substitute(column_name))
  result <- which(data[[column_name]] == value)
  
  if(is.na(value)) {result <- which(is.na(data[[column_name]] ))}

  # 값이 없으면 NA 반환
  if (length(result) == 0) {
    return("No matched value!")
  } else {
    return(result)
  }
}
