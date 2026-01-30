cor33 <- function(dataframe, target_var) {

if (base::missing(dataframe)) {
        cat("\033[1;32m## 상관계수 출력 -- \033[0m", "\n" )
	cat("\033[1;33m   cor33(df, 종속변수 ) \033[0m", "\n" )
        return(cat("   출력 결과는 |상관계수|가 큰 값부터 보여줌 " ))
	}
  
  # 변수명 따옴표 없이 입력 처리
  target_var <- deparse(substitute(target_var))
  target_var <- gsub("\"", "", target_var)
  target_var <- gsub("'", "", target_var)
  
  # 변수명 존재 여부 확인
  if (!target_var %in% names(dataframe)) {
    stop(paste0("에러: '", target_var, "' 변수가 데이터프레임에 존재하지 않습니다."))
  }
  
  # 데이터 전처리 (원본 보호)
  df_proc <- dataframe

c2n <- function(x) {
  if (base::missing(x)) {
    return(cat("  df$brandV2<-c2n(df$brand)"))
  }
  groups = unique(x)
  groups = sort(groups)
  tmp <- as.numeric(factor(x, levels = groups))
  return(tmp)
}


  # 문자형/팩터형 변수 숫자 변환 (c2n 적용)
  for (col in names(df_proc)) {
    if (is.character(df_proc[[col]]) || is.factor(df_proc[[col]])) {
      df_proc[[col]] <- c2n(df_proc[[col]])
    }
  }
  
  # y변수 데이터 추출
  y_data <- df_proc[[target_var]]
  
  # 결과를 저장할 리스트
  result_list <- list()
  
  # 상관계수 계산 루프
  for (col in names(df_proc)) {
    if (col == target_var) next 
    
    x_data <- df_proc[[col]]
    
    # NA 처리 (Pairwise complete)
    valid_idx <- !is.na(x_data) & !is.na(y_data)
    clean_x <- x_data[valid_idx]
    clean_y <- y_data[valid_idx]
    
    n_count <- length(clean_x)
    
    # 데이터 2개 이상, 분산 > 0 일 때 계산
    if (n_count >= 2 && sd(clean_x) > 0 && sd(clean_y) > 0) {
      cor_val <- cor(clean_x, clean_y)
      
      result_list[[length(result_list) + 1]] <- list(
        name = col,
        cor = cor_val,
        n = n_count
      )
    }
  }
  
  if (length(result_list) == 0) {
    cat("계산 가능한 상관계수가 없습니다.\n")
    return(invisible(NULL))
  }
  
  # 리스트를 데이터프레임으로 변환
  res_df <- do.call(rbind, lapply(result_list, as.data.frame))
  
  # [수정된 부분] 상관계수의 절대값(abs)을 기준으로 내림차순 정렬
  # 원본 값(res_df$cor)은 그대로 두고 정렬 순서만 절대값으로 판단합니다.
  res_df <- res_df[order(abs(res_df$cor), decreasing = TRUE), ]
  
  # 출력 포맷 생성
  output_items <- paste0(round(res_df$cor, 3), "(", res_df$name, ", ", res_df$n, ")")
  
  cat(paste0("dependent variable: ", target_var, "\n"))
  cat("(Sorted by Absolute Correlation Strength)\n")
  cat("-----------------------------------------\n")

  # 4개씩 출력
  for (i in seq_along(output_items)) {
    cat(output_items[i], "\t") 
    
    if (i %% 4 == 0) {
      cat("\n")
    }
  }
  if (length(output_items) %% 4 != 0) cat("\n")
}