rm_outlier <- function(x, option = 1) {

  if (base::missing(x)) {
	cat("  # 숫자 변수에 있는 콤마 제거 후 변수를 numeric으로 지정 ----- ", '\n')
	cat("    df_clean <- df[ rm_outlier(df$inc) & rm_outlier(df$asset), ] ", '\n')
	return( cat("    rm_outlier(df, 2) # 극단치가 있는 변수명/행번호 출력 ", '\n') )
	}

  # 1. 내부 함수: 단일 벡터에 대해 이상치 판별 (TRUE/FALSE 반환)
  check_vec <- function(vec) {
    if (!is.numeric(vec)) return(rep(TRUE, length(vec))) # 숫자가 아니면 모두 정상 처리
    Q1 <- quantile(vec, 0.25, na.rm = TRUE)
    Q3 <- quantile(vec, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    vec >= (Q1 - 1.5 * IQR) & vec <= (Q3 + 1.5 * IQR)
  }

  # 2. 입력이 데이터 프레임인 경우 처리
  if (is.data.frame(x)) {
    # 모든 컬럼에 대해 이상치 판별 수행
    result_df <- as.data.frame(lapply(x, check_vec))
    
    if (option == 1) {
      return(result_df) # TRUE/FALSE 데이터 프레임 반환
      
    } else if (option == 2) {
      # 이상치(FALSE)가 있는 위치 찾기
      outlier_list <- list()
      
      for (col_name in names(result_df)) {
        # 해당 컬럼에서 FALSE(이상치)인 행 번호 추출
        outlier_rows <- which(result_df[[col_name]] == FALSE)
        
        # 이상치가 있다면 결과 리스트에 추가
        if (length(outlier_rows) > 0) {
          outlier_list[[length(outlier_list) + 1]] <- data.frame(
            Variable = col_name,    # 여기가 핵심: 'df'가 아닌 '컬럼명' 저장
            Row_Number = outlier_rows
          )
        }
      }
      
      # 결과 합치기
      if (length(outlier_list) > 0) {
        return(do.call(rbind, outlier_list))
      } else {
        return("이상치가 없습니다.")
      }
    }
    
  } else {
    # 3. 입력이 단일 벡터인 경우 처리 (기존 로직 유지)
    res <- check_vec(x)
    if (option == 1) return(res)
    else if (option == 2) {
      outlier_idx <- which(res == FALSE)
      if(length(outlier_idx) > 0) {
        return(data.frame(Variable = deparse(substitute(x)), Row_Number = outlier_idx))
      } else { return("이상치가 없습니다.") }
    }
  }
}