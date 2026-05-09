summary_mlogit <- function(model) {
  suppressPackageStartupMessages(library("dplyr"))
  
  # 계수 및 통계량 계산
  coef_matrix <- coef(model)
  se_matrix <- summary(model)$standard.errors
  z_matrix <- coef_matrix / se_matrix
  p_matrix <- (1 - pnorm(abs(z_matrix), 0, 1)) * 2
  
  all_results <- list()
  
  # 각 카테고리별 출력 및 데이터 수집
  for (i in 1:nrow(coef_matrix)) {
    category <- rownames(coef_matrix)[i]
    
    # 카테고리 시작 전 한 줄 띄우기
    cat("\n--------------------------------------------------")
    cat("\nResults for category:", category, "\n\n") 
    
    results <- data.frame(
      Category = category,
      Variable = colnames(coef_matrix),
      Estimate = coef_matrix[i, ],
      Std.Er = se_matrix[i, ],
      z_value = z_matrix[i, ],
      p_value = p_matrix[i, ]
    )
    
    # 콘솔 출력용 가독성 정리 (mutate 사용)
    print_results <- results %>% 
      mutate(across(where(is.numeric), ~round(., 4)))
    
    print(print_results)
    
    # 카테고리 종료 후 한 줄 띄우기
    cat("\n") 
    
    all_results[[i]] <- results
  }
  
  # 전체 데이터프레임 병합 (table22 전달용)
  final_df <- do.call(rbind, all_results)
  
  # 모델 전체 요약 출력
  cat("==================================================")
  cat("\n<< Model Summary >>\n")
  cat(" Number of observations:", nrow(model$fitted.values), "\n")
  cat(" AIC:", round(AIC(model), 2), "\n")
  cat(" BIC:", round(BIC(model), 2), "\n\n")
  
  # table22에서 사용할 수 있도록 데이터 반환
  return(final_df)
}
