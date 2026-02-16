mkdlog <- function(dataset_name, ...) {
  
  # 1. 인자가 없을 경우 도움말 출력
  if (base::missing(dataset_name)) {
    cat(" 사용법: df2 <- mkdlog(dataset, var1, var2, var3)\n")
    cat(" \033[1;34m# 입력된 변수들에 로그를 취한 후 1차 차분한 결과를 추가합니다.\033[0m\n")
    cat(" \033[1;34m# 로그 변수명: log_변수명 / 로그차분 변수명: dlog_변수명\033[0m\n")
    return(invisible(NULL))
  }

  # 2. 따옴표 없이 입력된 변수명들을 문자열 벡터로 추출
  var_names <- as.character(substitute(list(...)))[-1]
  
  # 변수 입력이 없는 경우 처리
  if (length(var_names) == 0) {
    stop("로그 및 차분을 수행할 변수명을 입력해주세요.")
  }

  # 결과물을 담을 데이터프레임 복사
  res_df <- dataset_name

  # 3. 개별 변수에 대해 반복 작업 수행
  for (v_name in var_names) {
    
    # 데이터셋에 해당 변수가 있는지 확인
    if (!(v_name %in% names(dataset_name))) {
      cat(paste0(" 경고: '", v_name, "' 변수가 데이터셋에 없습니다. 건너뜁니다.\n"))
      next
    }

    # 원본 데이터 추출
    original_val <- dataset_name[[v_name]]
    
    # 로그 변환용 임시 벡터 생성
    log_val <- rep(NA, length(original_val))

    # 4. 로그 변환 로직 (기존 로직 유지)
    for (i in 1:length(original_val)) {
      val <- original_val[i]
      
      if (is.na(val)) {
        log_val[i] <- NA
      } else if (val <= 0) {
        # 0 이하인 경우 로그 불가
        log_val[i] <- NA
      } else if (val > 0 && val < 1) {
        # 0<x<1 인 경우 기존 로직에 따라 NA 처리 (필요시 log(val)로 수정 가능)
        log_val[i] <- NA
      } else {
        log_val[i] <- log(val)
      }
    }

    # 5. 변수 생성 및 병합
    log_col_name <- paste0("log_", v_name)
    dlog_col_name <- paste0("dlog_", v_name)

    # 로그 취한 값 추가
    res_df[[log_col_name]] <- log_val
    
    # 로그 차분(Difference) 값 추가: 현재 로그값 - 이전 로그값
    # dplyr 없이 기본 함수 diff를 사용하거나 lag를 직접 계산
    res_df[[dlog_col_name]] <- c(NA, diff(log_val))
    
    cat(paste0(" 처리완료: ", v_name, " -> ", log_col_name, ", ", dlog_col_name, "\n"))
  }

  return(res_df)
}