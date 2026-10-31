newplot <- function(data, x, y, col = "black", new = FALSE, ...) {
  # NA가 있어도 실행되는 plot. NA 행 번호를 화면에 알려주고, 나머지 행으로 그림을 그린다.
  # new = TRUE이면 그린 뒤 par(new = TRUE)를 실행하여 다음 plot/newplot이 같은 화면에 겹쳐 그려진다.
  # new = FALSE이면 par(new = TRUE)를 실행하지 않는다.

  if (base::missing(data)) {
     cat("\033[1;32m  # 명령문: newplot(데이터셋, x변수, y변수, 컬러, par(new=FALSE) ) \033[0m ", '\n')
     cat("  # new: par(new = TRUE) 실행 여부 (TRUE/FALSE, 기본값 FALSE)", '\n')
     cat("  newplot(df, Age, Income)", '\n')
     cat("  newplot(df, Age, Income, red)", '\n')
     cat("  newplot(df, Age, Income, TRUE, 'red')", '\n')
     cat("  ", '\n')
     cat("  • type = 'p': 포인트(Points)만 그리기 (기본값)", '\n')
     cat("  • type = 'l': 라인(Lines)만 그리기", '\n')
     cat("  • type = 'b': 포인트와 라인을 둘 다(Both) 그리기 (서로 연결됨)", '\n')
     cat("  • type = 'o': 포인트와 라인을 중첩(Overplotted)해서 그리기 (라인이 포인트를 통과함)", '\n')
     cat("  • type = 'h': 수직선(Histogram-like) 형태로 그리기", '\n')
     cat("  • type = 's': 계단식(Steps)으로 그리기", '\n')
     cat("  • type = 'n': 아무것도 그리지 않기 (축과 프레임만 생성하고 나중에 추가할 때 사용)", '\n')
     cat("  ", '\n')
     cat("  • pch = 1: 빈 원 (기본값)", '\n')
     cat("  • pch = 16: 채워진 원", '\n')
     cat("  • pch = 17: 채워진 삼각형", '\n')
     cat("  • pch = 19: 큰 채워진 원", '\n')
     cat("  ", '\n')
     cat("  • lty = 1: 실선 (Solid, 기본값)", '\n')
     cat("  • lty = 2: 대시선 (Dashed)", '\n')
     cat("  • lty = 3: 점선 (Dotted)", '\n')
     cat("  • lty = 4: 점-대시 혼합선 (Dot-dash)", '\n')
     cat("  ", '\n')
     cat("  • lwd = 2: 선의 두께를 기본값의 2배로 설정", '\n')
     cat("  ", '\n')
     cat("  ", '\n')
     return(cat("      ", '\n'))  }

  x_expr <- substitute(x)
  y_expr <- substitute(y)
  x_name <- if (is.character(x_expr)) x_expr else deparse(x_expr)
  y_name <- if (is.character(y_expr)) y_expr else deparse(y_expr)

  for (v in c(x_name, y_name)) {
    if (!v %in% names(data)) stop("'", v, "' 변수가 데이터셋에 없습니다.")
  }

  xv <- data[[x_name]]
  yv <- data[[y_name]]

  for (v in c(x_name, y_name)) {
    na_rows <- which(is.na(data[[v]]))
    if (length(na_rows) > 0) {
      cat("NA 행 번호 (", v, "): ", paste(na_rows, collapse = ","), '\n', sep = "")
    }
  }

  ok <- !is.na(xv) & !is.na(yv)
  plot(xv[ok], yv[ok], xlab = x_name, ylab = y_name, col = col, ...)
  if (isTRUE(new)) par(new = TRUE)
  invisible(which(!ok))
}
