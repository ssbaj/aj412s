mkdate <- function(df, start_y, type = "y", start_p = 1) {
  # 1. 안내 메시지 (데이터프레임 누락 시)
  if (base::missing(df)) {
    cat("Usage: mkdate_series(df, start_y, type=y/m/q, start_p=시작월/분기)\n")
    cat(" - type=y: 연 단위 (12월 31일 기준)\n")
    cat(" - type=m: 월 단위 (start_p에 시작 월 입력, 기본값 1)\n")
    cat(" - type=q: 분기 단위 (start_p에 시작 분기 입력 1~4, 기본값 1)\n")
    return(invisible(NULL))
  }

  # --- 추가된 부분: 따옴표 없이 입력된 인자를 문자로 변환 ---
  # 사용자가 type = m 처럼 입력하면 이를 "m"으로 바꿉니다.
  # 이미 "m"으로 입력한 경우에도 문제없이 작동합니다.
  type_str <- as.character(substitute(type))
  # -----------------------------------------------------

  df <- as.data.frame(df)
  n <- nrow(df)
  dates <- vector("character", n)
  
  # 2. 날짜 생성 로직 (이제 type 대신 type_str 사용)
  if (type_str == "y") {
    for (i in 1:n) {
      dates[i] <- sprintf("%d-12-31", start_y + i - 1)
    }
    
  } else if (type_str == "m") {
    curr_y <- start_y
    curr_m <- start_p
    for (i in 1:n) {
      dates[i] <- sprintf("%d-%02d-01", curr_y, curr_m)
      curr_m <- curr_m + 1
      if (curr_m > 12) {
        curr_m <- 1
        curr_y <- curr_y + 1
      }
    }
    
  } else if (type_str == "q") {
    curr_y <- start_y
    curr_q <- start_p
    q_months <- c(3, 6, 9, 12)
    
    for (i in 1:n) {
      dates[i] <- sprintf("%d-%02d-01", curr_y, q_months[curr_q])
      curr_q <- curr_q + 1
      if (curr_q > 4) {
        curr_q <- 1
        curr_y <- curr_y + 1
      }
    }
  } else {
    stop("type은 y, m, q 중 하나여야 합니다.")
  }

  # 3. 데이터 결합 및 정렬
  df$DATE <- as.Date(dates)
  
  if (requireNamespace("dplyr", quietly = TRUE)) {
    df <- dplyr::relocate(df, DATE)
  } else {
    df <- df[, c("DATE", setdiff(names(df), "DATE"))]
  }
  
  return(df)
}