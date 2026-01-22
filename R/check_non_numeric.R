check_non_numeric <- function(df) {
  # 결과를 담을 빈 데이터프레임 생성 (순서: Column, Row, Value)
  report <- data.frame(
    Column = character(),
    Row = integer(),
    Value = character(),
    stringsAsFactors = FALSE
  )
  
  # 데이터셋의 모든 열을 순회
  for (col_name in names(df)) {
    
    # 1. 값 가져오기 & 수치 변환 시도
    original_vals <- as.character(df[[col_name]])
    numeric_vals <- suppressWarnings(as.numeric(original_vals))
    
    # 2. 문제가 있는 위치 찾기 (변환결과 NA & 원본값 !NA)
    bad_indices <- which(is.na(numeric_vals) & !is.na(original_vals))
    
    # 3. 발견된 경우 리포트에 추가
    if (length(bad_indices) > 0) {
      temp_df <- data.frame(
        Column = col_name,
        Row = bad_indices,
        Value = original_vals[bad_indices],
        stringsAsFactors = FALSE
      )
      report <- rbind(report, temp_df)
    }
  }
  
  # 결과 가공
  if (nrow(report) > 0) {
    # 보기 좋게 출력용 데이터프레임 생성
    display_report <- report
    
    # 2번째 행부터 끝까지 돌면서, 바로 윗행과 Column명이 같으면 빈칸 처리
    if (nrow(display_report) > 1) {
      for (i in 2:nrow(display_report)) {
        # 주의: 비교는 원본(report) 기준으로 해야 정확합니다.
        if (report$Column[i] == report$Column[i-1]) {
          display_report$Column[i] <- ""
        }
      }
    }
    
    cat(paste0(">>> 총 ", nrow(report), "개의 수치가 아닌 값이 발견되었습니다. <<<\n"))
    return(display_report)
    
  } else {
    cat("이상 없음: 모든 데이터가 수치로 변환 가능합니다.\n")
    return(NULL)
  }
}



