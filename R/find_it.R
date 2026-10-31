find_it <- function(data, column_name, value) {
  # 주어진 변수명의 컬럼에서 value가 있는 행 번호를 '데이터셋, 변수명, 행' 형태로 출력

  if (base::missing(data)) {
     cat("\033[1;32m  # 명령문: find_it(데이터셋, 변수명, 변수값) \033[0m ", '\n')
     cat("\033[1;32m  # 명령문: 변수값이 문자이면 따옴표로 감싸고, 숫자이면 그대로 입력 \033[0m ", '\n')
     cat("  find_it(df, Name, 'David')",'\n')
     cat("  find_it(df, Age, 36)",'\n')
     return(cat("  find_it(df, ID, NA)",'\n'))  }

  data_name   <- deparse(substitute(data))
  column_expr <- substitute(column_name)
  column_name <- if (is.character(column_expr)) column_expr else deparse(column_expr)

  if (!column_name %in% names(data)) {
    stop("'", column_name, "' 변수가 ", data_name, " 에 없습니다.")
  }

  if (is.na(value)) {
    result <- which(is.na(data[[column_name]]))
  } else {
    result <- which(data[[column_name]] == value)
  }

  if (length(result) == 0) {
    cat("No matched value!", '\n')
    return(invisible(integer(0)))
  }
  cat("   ", '\n')
  cat("찾는 자료수: ", length(result), '\n', sep = "")
  cat("   ", '\n')
  cat("행번호: ", paste(result, collapse = ","), '\n', sep = "")
  cat("   ", '\n')
  invisible(result)
}
