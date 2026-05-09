table22 <- function(input_data, filenames = NULL, model_for_r2 = NULL) {
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  library(writexl)
  
  # 1. 파일명 자동 생성 (초 단위 포함)
  if (is.null(filenames)) {
    time_stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    filenames <- paste0("Re", time_stamp, ".xlsx")
  }
  
  # 2. 입력 데이터 판별 및 변환
  if (inherits(input_data, "glm")) {
    # --- 로짓 분석(glm) ---
    res_mat <- summary(input_data)$coefficients
    final_table <- data.frame(
      Variable  = rownames(res_mat),
      Estimate  = round(as.numeric(res_mat[, 1]), 4),
      Std_Error = round(as.numeric(res_mat[, 2]), 4),
      t_z_value = round(as.numeric(res_mat[, 3]), 2),
      P_Value   = round(as.numeric(res_mat[, 4]), 4),
      stringsAsFactors = FALSE
    )
    # 요약 정보 행 생성 (열 이름 일치시킴)
    summary_info <- data.frame(
      Variable  = c("AIC", "Observations"),
      Estimate  = c(round(AIC(input_data), 2), nrow(input_data$model)),
      Std_Error = NA, t_z_value = NA, P_Value = NA,
      stringsAsFactors = FALSE
    )
    final_table <- rbind(final_table, summary_info)
    message("로짓 분석(glm) 결과를 처리했습니다.")
    
  } else if (inherits(input_data, "lm")) {
    # --- 일반 회귀분석(lm) ---
    res_mat <- summary(input_data)$coefficients
    final_table <- data.frame(
      Variable  = rownames(res_mat),
      Estimate  = round(as.numeric(res_mat[, 1]), 4),
      Std_Error = round(as.numeric(res_mat[, 2]), 4),
      t_z_value = round(as.numeric(res_mat[, 3]), 2),
      P_Value   = round(as.numeric(res_mat[, 4]), 4),
      stringsAsFactors = FALSE
    )
    # R2 행 생성 (열 이름을 final_table과 정확히 일치시켜 rbind 에러 방지)
    r2_rows <- data.frame(
      Variable  = c("R-squared", "Adj.R-squared"),
      Estimate  = c(round(summary(input_data)$r.squared, 4), round(summary(input_data)$adj.r.squared, 4)),
      Std_Error = NA, t_z_value = NA, P_Value = NA,
      stringsAsFactors = FALSE
    )
    final_table <- rbind(final_table, r2_rows)
    message("일반 회귀분석(lm) 결과를 처리했습니다.")
    
  } else if (inherits(input_data, "coeftest")) {
    # --- Robust 결과(coeftest) ---
    res_mat <- as.matrix(input_data)
    final_table <- data.frame(
      Variable  = rownames(res_mat),
      Estimate  = round(as.numeric(res_mat[, 1]), 4),
      Std_Error = round(as.numeric(res_mat[, 2]), 4),
      t_z_value = round(as.numeric(res_mat[, 3]), 2),
      P_Value   = round(as.numeric(res_mat[, 4]), 4),
      stringsAsFactors = FALSE
    )
    
  } else {
    # --- summary_mlogit 또는 desc22 결과 ---
    final_table <- as.data.frame(input_data)
    if (!"Variable" %in% colnames(final_table)) {
      final_table <- cbind(Variable = rownames(final_table), final_table)
    }
  }
  
  # 3. 유의성 별표 (*) 추가
  # 대소문자 구분 없이 P_Value 컬럼 찾기
  p_col <- grep("P_Value|p_value", colnames(final_table), value = TRUE)
  if (length(p_col) > 0) {
    final_table$Signif <- ifelse(final_table[[p_col[1]]] < 0.01, "***", 
                                 ifelse(final_table[[p_col[1]]] < 0.05, "**", 
                                        ifelse(final_table[[p_col[1]]] < 0.1, "*", "")))
  }
  
  # 4. 저장 및 출력
  rownames(final_table) <- NULL 
  print(final_table)
  write_xlsx(final_table, path = filenames)
  cat("\n[성공] 파일 저장 완료:", filenames, "\n")
}

