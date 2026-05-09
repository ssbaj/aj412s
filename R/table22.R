table22 <- function(input_data, filenames = NULL, model_for_r2 = NULL) {
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  library(writexl)
  
  # 1. 파일명 자동 생성 (초 단위)
  if (is.null(filenames)) {
    time_stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    filenames <- paste0("Re", time_stamp, ".xlsx")
  }
  
  final_table <- NULL
  
  # 2. 클래스별 데이터 처리
  if (inherits(input_data, "multinom")) {
    # [수정] multinom 객체 직접 처리 로직 추가
    message("다항 로짓(multinom) 결과를 직접 처리합니다.")
    sum_obj <- summary(input_data)
    coef_matrix <- as.matrix(sum_obj$coefficients)
    se_matrix <- as.matrix(sum_obj$standard.errors)
    
    # 카테고리가 1개인 경우 처리
    if (nrow(coef_matrix) == 1) rownames(coef_matrix) <- "Response"
    
    z_matrix <- coef_matrix / se_matrix
    p_matrix <- (1 - pnorm(abs(z_matrix), 0, 1)) * 2
    
    combined_list <- list()
    for (i in 1:nrow(coef_matrix)) {
      combined_list[[i]] <- data.frame(
        Category  = rownames(coef_matrix)[i],
        Variable  = colnames(coef_matrix),
        Estimate  = round(as.numeric(coef_matrix[i, ]), 4),
        Std_Error = round(as.numeric(se_matrix[i, ]), 4),
        t_z_value = round(as.numeric(z_matrix[i, ]), 2),
        P_Value   = round(as.numeric(p_matrix[i, ]), 4),
        stringsAsFactors = FALSE
      )
    }
    final_table <- do.call(rbind, combined_list)
    
    # 모델 정보 추가
    summary_info <- data.frame(
      Category = "Model Summary", Variable = c("Observations", "AIC", "BIC"),
      Estimate = c(nrow(input_data$fitted.values), round(AIC(input_data), 2), round(BIC(input_data), 2)),
      Std_Error = NA, t_z_value = NA, P_Value = NA, stringsAsFactors = FALSE
    )
    final_table <- rbind(final_table, summary_info)
    
  } else if (inherits(input_data, "glm")) {
    # 로짓 분석(glm)
    res_mat <- summary(input_data)$coefficients
    final_table <- data.frame(
      Variable = rownames(res_mat),
      Estimate = round(as.numeric(res_mat[, 1]), 4),
      Std_Error = round(as.numeric(res_mat[, 2]), 4),
      t_z_value = round(as.numeric(res_mat[, 3]), 2),
      P_Value = round(as.numeric(res_mat[, 4]), 4),
      stringsAsFactors = FALSE
    )
    summary_info <- data.frame(
      Variable = c("AIC", "Observations"),
      Estimate = c(round(AIC(input_data), 2), nrow(input_data$model)),
      Std_Error = NA, t_z_value = NA, P_Value = NA, stringsAsFactors = FALSE
    )
    final_table <- rbind(final_table, summary_info)
    
  } else if (inherits(input_data, "lm")) {
    # 일반 회귀분석(lm)
    res_mat <- summary(input_data)$coefficients
    final_table <- data.frame(
      Variable = rownames(res_mat),
      Estimate = round(as.numeric(res_mat[, 1]), 4),
      Std_Error = round(as.numeric(res_mat[, 2]), 4),
      t_z_value = round(as.numeric(res_mat[, 3]), 2),
      P_Value = round(as.numeric(res_mat[, 4]), 4),
      stringsAsFactors = FALSE
    )
    r2_rows <- data.frame(
      Variable = c("R-squared", "Adj.R-squared"),
      Estimate = c(round(summary(input_data)$r.squared, 4), round(summary(input_data)$adj.r.squared, 4)),
      Std_Error = NA, t_z_value = NA, P_Value = NA, stringsAsFactors = FALSE
    )
    final_table <- rbind(final_table, r2_rows)
    
  } else if (inherits(input_data, "coeftest")) {
    # Robust 결과
    res_mat <- as.matrix(input_data)
    final_table <- data.frame(
      Variable = rownames(res_mat),
      Estimate = round(as.numeric(res_mat[, 1]), 4),
      Std_Error = round(as.numeric(res_mat[, 2]), 4),
      t_z_value = round(as.numeric(res_mat[, 3]), 2),
      P_Value = round(as.numeric(res_mat[, 4]), 4),
      stringsAsFactors = FALSE
    )
    
  } else {
    # summary_mlogit 결과물(data.frame) 또는 desc22 결과물
    final_table <- as.data.frame(input_data)
    if (!"Variable" %in% colnames(final_table)) {
      final_table <- cbind(Variable = rownames(final_table), final_table)
    }
  }
  
  # 3. 유의성 별표 (*) 추가
  p_col <- grep("P_Value|p_value", colnames(final_table), value = TRUE)
  if (length(p_col) > 0) {
    final_table$Signif <- ifelse(final_table[[p_col[1]]] < 0.01, "***", 
                                 ifelse(final_table[[p_col[1]]] < 0.05, "**", 
                                        ifelse(final_table[[p_col[1]]] < 0.1, "*", "")))
  }
  
  # 4. 최종 출력 및 저장
  rownames(final_table) <- NULL
  print(head(final_table, 20))
  write_xlsx(final_table, path = filenames)
  cat("\n[성공] 파일 저장 완료:", filenames, "\n")
}
