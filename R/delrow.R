# delrow.R
# 사용법:
#   source("delrow.R")
#   delrow(df, 3)          # df <- df[-3, ]
#   delrow(df, c(3, 5, 8)) # df <- df[-c(3, 5, 8), ]
#   delrow(df, 3, run = FALSE) # 명령문만 출력하고 실행은 하지 않는다
#
# data.frame 이름과 행번호를 주면 해당 행을 삭제하는 R 명령문을 만들어
# 화면에 출력한 뒤 바로 실행하여 df에서 해당 행을 삭제하고 행 이름을 1번부터 다시 매긴다 (run = FALSE면 출력만).

delrow <- function(df, rows, run = TRUE) {

  if (base::missing(df)) {
        cat("   ", "\n")
	cat(" \033[1;31mdelrow(df, 3)              # df <- df[-3, ]  \033[0m", "\n")
        cat(" \033[1;31mdelrow(df, c(3, 5, 8))     # df <- df[-c(3, 5, 8), ]  \033[0m", "\n")
        cat("   ", "\n")
        return(invisible(NULL))
    }

  
  dname <- deparse(substitute(df))[1]
  stopifnot(is.data.frame(df), is.numeric(rows), length(rows) > 0)

  rows <- sort(unique(as.integer(rows)))
  if (any(rows < 1 | rows > nrow(df)))
    stop(sprintf("행번호는 1 ~ %d 범위여야 합니다.", nrow(df)))

  idx <- if (length(rows) == 1) as.character(rows)
         else sprintf("c(%s)", paste(rows, collapse = ", "))
  cmd <- sprintf("%s <- %s[-%s, , drop = FALSE]; rownames(%s) <- NULL", dname, dname, idx, dname)

  cat(cmd, "\n")
  if (run) eval(parse(text = cmd), envir = parent.frame())
  invisible(cmd)
}
