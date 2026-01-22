chk22 <- function(data) {
  
  if (base::missing(data)) {
    cat("    ", '\n')
    cat("\033[1;32m  # chk22는 non numeric cell을 찾아 리포팅하는 함수 ", '\n')
    cat("\033[1;32m  # 사용예: chk22(데이터셋) 또는, chk22(df$변수)  ", '\n')
    cat("\033[1;32m  ---------------------------------------------- ", '\n')
    cat("   ", '\n')
    cat("\033[1;33m  ## Non numeric cell을 NA로 변경하는 방법 ## ", '\n')
    cat("\033[1;33m  library(dplyr) ", '\n')
    cat("\033[1;33m  df <- df %>% mutate(across(c(변수1, 변수2, 변수3), ~ suppressWarnings(as.numeric(as.character(.)))))  ", '\n')
    return(cat("   ", '\n') )
  }
  
    # --- [1] 입력 데이터 타입 확인 및 전처리 ---
  # 데이터프레임이면 그대로 사용, 벡터(df$V123)면 데이터프레임으로 변환
  if (is.data.frame(data)) {
    df_check <- data
  } else {
    # 벡터인 경우, 입력한 변수명(예: "df$V123")을 가져옴
    var_name <- deparse(substitute(data))
    # 이름이 너무 길거나 복잡하면 기본값 사용
    if (length(var_name) > 1) var_name <- "Input_Vector"
    
    # 1열짜리 데이터프레임으로 변환
    df_check <- data.frame(data, stringsAsFactors = FALSE)
    names(df_check) <- var_name
  }
  
  # --- [2] 비수치 데이터 찾기 (기존 로직) ---
  report <- data.frame(
    Column = character(),
    Row = integer(),
    Value = character(),
    stringsAsFactors = FALSE
  )
  
  for (col_name in names(df_check)) {
    
    original_vals <- as.character(df_check[[col_name]])
    numeric_vals <- suppressWarnings(as.numeric(original_vals))
    
    # 숫자로 변환했더니 NA가 됐는데, 원래 값은 NA가 아니었던 행 찾기
    bad_indices <- which(is.na(numeric_vals) & !is.na(original_vals))
    
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
  
  # --- [3] 결과 출력 및 서식 정리 ---
  if (nrow(report) > 0) {
    display_report <- report
    
    # Column 이름 중복 제거 (깔끔하게 보이기 위함)
    if (nrow(display_report) > 1) {
      for (i in 2:nrow(display_report)) {
        if (report$Column[i] == report$Column[i-1]) {
          display_report$Column[i] <- ""
        }
      }
    }
    
    cat(paste0(">>> 총 ", nrow(report), "개의 수치가 아닌 값이 발견되었습니다. <<<\n"))
    return(display_report)
    
  } else {
    cat("이상 없음: 입력한 데이터는 모두 수치로 변환 가능합니다.\n")
    return( cat(" ", "\n") )
  }
}

