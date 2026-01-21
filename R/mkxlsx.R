mkxlsx <- function(dataset, file_path) {

  if (base::missing(dataset)) {
    cat("\033[1;32m# 데이터셋을 엑셀 xlsx 파일로 저장하기 ----- \033[0m\n")
    cat("\033[1;32m# mkxlsx(데이터셋, 새로운파일명.xlsx ) \033[0m\n")
    return(invisible(NULL))
  }

  # �따옴표 없이 입력된 file_path를 문자열로 변환
  file_path <- deparse(substitute(file_path))

  #  확장자 자동 보정 (.xlsx 없으면 추가)
  if (!grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
    file_path <- paste0(file_path, ".xlsx")
  }

  # writexl 패키지 설치 확인
  if (!requireNamespace("writexl", quietly = TRUE)) {
    install.packages("writexl")
  }

  # 패키지 로드 (메시지 숨김)
  suppressPackageStartupMessages(library(writexl))

  # xlsx 파일 저장
  tryCatch({
    write_xlsx(dataset, path = file_path)
    message("  Excel 파일 저장 완료: '", file_path, "'")
  }, error = function(e) {
    stop(paste0("파일 저장 중 오류가 발생함: ", e$message))
  })
}
