tables22 <- function() {
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  library(writexl)
  library(tcltk)

  # 1. 내부 함수: files22SE (공백 구분자 전용 로딩)
  files22SE <- function() {
    tt_parent <- tktoplevel()
    tkwm.withdraw(tt_parent)
    tcl("wm", "attributes", tt_parent, "-topmost", TRUE)
    
    f_path <- tclvalue(tkgetOpenFile(parent = tt_parent, 
                                     title = "통계 결과 텍스트 파일 선택",
                                     filetypes = "{{Text Files} {.txt}} {{All Files} {*}}"))
    tkdestroy(tt_parent)
    
    if (f_path == "") return(NULL)

    # UTF-8 인코딩으로 전체 행 읽기
    all_content <- readLines(f_path, encoding = "UTF-8", warn = FALSE)
    
    # 첫 숫자가 등장하는 행 번호 찾기 (데이터 시작점 포착)
    first_digit_idx <- grep("[0-9]", all_content)[1]
    
    if (is.na(first_digit_idx)) {
      message("파일 내에서 수치 데이터를 찾을 수 없습니다.")
      return(NULL)
    }
    
    # 숫자 시작 지점부터 추출
    valid_text <- all_content[first_digit_idx:length(all_content)]
    
    # [수정 핵심] sep = "" 은 모든 종류의 공백(스페이스, 탭 등)을 구분자로 처리하며
    # 연속된 공백도 하나로 간주하여 열을 정확히 분리합니다.
    result <- read.table(text = valid_text, sep = "", fill = TRUE, 
                         stringsAsFactors = FALSE, check.names = FALSE,
                         header = FALSE)
    
    message(paste("Successfully loaded:", basename(f_path)))
    return(result)
  }

  # 2. 파일 로딩 실행
  input_data <- files22SE()
  if (is.null(input_data)) return(message("작업이 취소되었습니다."))

  # 3. 데이터프레임 변환 및 정리
  final_df <- as.data.frame(input_data, stringsAsFactors = FALSE)
  
  # 열 이름 부여 (Variable, Estimate, Std. Error, t value, Pr(>|t|), Signif 순)
  # 데이터의 실제 열 개수에 맞춰 유연하게 할당
  col_names <- c("Variable", "Estimate", "Std_Error", "t_value", "P_Value", "Signif")
  actual_cols <- ncol(final_df)
  colnames(final_df) <- col_names[1:actual_cols]
  
  # NA 값을 빈칸으로 정리 (별표가 없는 행 처리)
  final_df[is.na(final_df)] <- ""

  # 4. 자동 저장 (Re + 날짜시간분초)
  time_stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  filename <- paste0("Re", time_stamp, ".xlsx")
  
  write_xlsx(final_df, path = filename)

  # 5. 결과 출력
  cat("\n--------------------------------------------------\n")
  cat("[공백 구분 파싱 완료]\n")
  cat("저장 파일:", filename, "\n")
  cat("--------------------------------------------------\n")
  print(final_df)
  
  return(invisible(final_df))
}
