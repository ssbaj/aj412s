# mydir(): 폴더 선택 팝업을 띄워 선택한 폴더를 작업 폴더로 지정한다.
# 사용: mydir() 입력
mydir <- function() {
  # 작업 폴더를 선택하세요 (작업 폴더를 선택하세요)
  cap <- "작업 폴더를 선택하세요"

  valid <- function(p) {
    is.character(p) && length(p) == 1 && !is.na(p) && nzchar(p) && dir.exists(p)
  }
  try_pick <- function(f) {
    p <- tryCatch(f(), error = function(e) NULL)
    if (valid(p)) p else NULL
  }

  path <- NULL

  # 1) RStudio 에서 실행한 경우: rstudioapi (가장 안정적)
  if (is.null(path) && requireNamespace("rstudioapi", quietly = TRUE) &&
      tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE)) {
    path <- try_pick(function() rstudioapi::selectDirectory(caption = cap))
  }

  # 2) Windows 기본 폴더 선택창
  if (is.null(path) && .Platform$OS.type == "windows") {
    path <- try_pick(function() utils::choose.dir(caption = cap))
  }

  # 3) tcltk (기타 환경)
  if (is.null(path) && requireNamespace("tcltk", quietly = TRUE)) {
    path <- try_pick(function() as.character(tcltk::tk_choose.dir(caption = cap)))
  }

  # 취소한 경우
  if (is.null(path)) {
    message("취소되었습니다. 작업 폴더가 바뀌지 않았습니다.")
    return(invisible(NULL))
  }

  setwd(path)
  message("작업 폴더: ", getwd())
  invisible(getwd())
}
