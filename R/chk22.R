chk22 <- function(data, exclude = NULL) {
  
     if (base::missing(data)) {
      cat("   ", "\n")
      cat("  \033[1;33m사용예: chk22(data)  #조사를 위한 dataset 지정 \033[0m", "\n")
      cat("  \033[1;33m사용예: chk22(data, exclude=c(변수1, 변수2, 변수3)) #chk22조사에서 제외될 변수 선택 \033[0m", "\n")
      cat("  \033[1;33m사용예: chk22(data$변수1) #변수 1개만 조사 \033[0m", "\n")
      cat("  \033[1;33m#------------------------------------ \033[0m", "\n")
      cat("  \033[1;33m        \033[0m", "\n")
      cat("  \033[1;33m# numeric cell이 아닌 것을 모두 NA로 변경하기\033[0m", "\n")
      cat("  \033[1;33m# 모든 열에 적용 (주의: 이름, 주소 같은 문자열 열도 모두 NA가 됩니다!)\033[0m", "\n")
      cat("  \033[1;32m data <- data %>% \033[0m", "\n")
      cat("  \033[1;32m       mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.))))) \033[0m", "\n")
      cat("  \033[1;33m \033[0m", "\n")
      return(cat("   \n"))
    }


  # --- [1] 데이터 타입 확인 및 전처리 ---
  if (is.data.frame(data)) {
    df_check <- data
  } else {
    # 벡터인 경우
    var_name <- deparse(substitute(data))
    if (length(var_name) > 1) var_name <- "Vector"
    df_check <- data.frame(data, stringsAsFactors = FALSE)
    names(df_check) <- var_name
  }
  
  # --- [2] 제외할 변수 처리 (요청사항 1) ---
  if (!is.null(exclude)) {
    # 입력된 변수명들이 데이터셋에 있는지 확인하고 제거
    existing_cols <- names(df_check)
    target_cols <- setdiff(existing_cols, exclude)
    
    if (length(target_cols) == 0) {
      cat(">>> [경고] 모든 변수가 제외되었습니다. 검사할 항목이 없습니다.\n")
      return(NULL)
    }
    # 제외된 변수들을 뺀 나머지만 선택
    df_check <- df_check[, target_cols, drop = FALSE]
  }
  
  # --- [3] 비수치 데이터 찾기 ---
  report <- data.frame(
    Row = integer(),
    Column = character(),
    Value = character(),
    stringsAsFactors = FALSE
  )
  
  for (col in names(df_check)) {
    orig <- as.character(df_check[[col]])
    num_val <- suppressWarnings(as.numeric(orig))
    
    # 변환 실패(NA) & 원본은 NA 아님
    bad_idx <- which(is.na(num_val) & !is.na(orig))
    
    if (length(bad_idx) > 0) {
      report <- rbind(report, data.frame(
        Row = bad_idx,
        Column = col,
        Value = orig[bad_idx],
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # --- [4] 결과 출력 서식 설정 (요청사항 2: 행 기준) ---
  if (nrow(report) > 0) {
    # 1. 행(Row) 번호 순서로 정렬 (그 다음 칼럼명 순)
    report <- report[order(report$Row, report$Column), ]
    
    # 2. 출력용 데이터프레임 생성 (Row를 문자로 변환)
    disp <- report
    disp$Row <- as.character(disp$Row)
    
    # 3. 중복된 Row 번호는 빈칸("") 처리하여 가독성 높임
    if (nrow(disp) > 1) {
      for (i in 2:nrow(disp)) {
        # 윗 행과 Row 번호가 같으면 빈칸으로
        if (report$Row[i] == report$Row[i-1]) {
          disp$Row[i] <- ""
        }
      }
    }
    
    cat(paste0(">>> [발견] 수치가 아닌 값 ", nrow(report), "개 (행 기준 정렬) <<<\n"))
    return(disp)
    
  } else {
    cat(">>> [정상] 제외된 열을 뺀 나머지 모든 데이터가 수치 변환 가능합니다.\n")
    return(NULL)
  }
}
