panel_diff <- function(p_df) {

if (base::missing(p_df)) {
cat("   ", '\n')
cat("  # 1. 예제 df를 만들기위한 벡터 생성 --------- ", '\n')
cat("  index <- c('A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10', 'A11', 'A12') ", '\n')
cat("  area <- c('A', 'A', 'A', 'B', 'B', 'B', 'C', 'C', 'C', 'D', 'D', 'D') ", '\n')
cat("  year <- c(2001, 2002, 2003, 2001, 2002, 2003, 2001, 2002, 2003, 2001, 2002, 2003) ", '\n')
cat("  변수1 <- c(10, 14, 18, 21, 23, 25, 32, 36, 40, 42, 44, 46) ", '\n')
cat("  변수2 <- c(13, 68, 86, 23, 78, 98, 34, 79, 92, 53, 83, 100) ", '\n')
cat("   ", '\n')
cat("  # 2. 벡터들을 모아 데이터 프레임 생성 ", '\n')
cat("  df <- data.frame(index, ind2, date2, 변수1, 변수2) ", '\n')
cat("   ", '\n')
cat("  # plm패키지의 pdata.frame을 사용: 패널데이터 지정 -> 차분difference ", '\n')
cat("  # df 차분. 단, area, year, index, 문자변수는 차분 제외  ----------- ", '\n')
cat("   ", '\n')
cat("  library(plm) ", '\n')
cat("  panel_data <- pdata.frame(panel, index = c('area', 'year'))  ", '\n')
return(cat("  diffDataSet<-panel_diff(panel_data) ", '\n'))
  }

  
  if (!require("plm", quietly = TRUE)) {
  install.packages("plm")
  }

  # 1. pdata.frame의 속성에서 인덱스(area, year) 자동 파악
  idx_info <- attr(p_df, "index")
  idx_names <- names(idx_info)  # 보통 c("area", "year")가 추출됨
  
  # 2. 결과 저장용 리스트 생성
  result_list <- list()
  
  # 3. 모든 컬럼 순회
  for (col in names(p_df)) {
    
    # 해당 컬럼의 데이터 추출
    target_series <- p_df[[col]]
    
    # 조건: 인덱스 변수가 아니고 & 수치형 데이터인 경우만 차분 수행
    if (!(col %in% idx_names) && is.numeric(target_series)) {
      
      # pseries 객체에 diff를 적용하면 패널 구조(area별 year)를 인식하여 차분함
      # lag를 지정하지 않으면 기본값인 1(바로 전 시점과의 차이)로 계산됨
      diff_val <- diff(target_series)
      
      # 차분된 변수명 앞에 'd' 추가
      result_list[[paste0("d", col)]] <- diff_val
      
    } else {
      # 인덱스(area, year)나 문자 변수는 연산 없이 그대로 유지하되 이름 앞에 'd' 추가
      result_list[[paste0("d", col)]] <- target_series
    }
  }
  
  # 4. 리스트를 데이터프레임으로 변환
  final_df <- as.data.frame(result_list)
  
  # 5. 기존 패널 인덱스 구조를 유지하며 pdata.frame 재설정
  # 인덱스 변수명에도 'd'가 붙었으므로 이를 반영함
  new_idx_names <- paste0("d", idx_names)
  final_p_df <- pdata.frame(final_df, index = new_idx_names)
  
  return(final_p_df)
}

