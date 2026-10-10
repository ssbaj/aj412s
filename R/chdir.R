# chdir(): 폴더 선택 팝업을 띄워 선택한 폴더를 작업 폴더로 지정한다.
# 사용: source("chdir.R") 후 chdir() 입력
chdir <- function() {
  # 폴더 선택 팝업
  if (.Platform$OS.type == "windows") {
    path <- utils::choose.dir(caption = "작업 폴더를 선택하세요")
  } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    path <- rstudioapi::selectDirectory("작업 폴더를 선택하세요")
  } else {
    path <- tcltk::tk_choose.dir(caption = "작업 폴더를 선택하세요")
  }

  # 취소한 경우
  if (is.null(path) || is.na(path) || path == "") {
    message("취소되었습니다. 작업 폴더가 바뀌지 않았습니다.")
    return(invisible(NULL))
  }

  setwd(path)
  message("작업 폴더: ", getwd())
  invisible(getwd())
}
