table22 <- function(input_data, filenames = NULL, model_for_r2 = NULL) {
  if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
  library(writexl)
  
  # 1. 파일명 자동 생성 (초 단위)
  if (is.null(filenames)) {
    time_stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    filenames <- paste0("Re", time_stamp, ".xlsx")
  }
  
  # =========================================================================
  # [수정] 상관계수 감지 시 데이터프레임 경고(Warning) 완벽 차단
  # =========================================================================
  is_cor <- FALSE
  try({
    if (!is.null(dim(input_data)) && ncol(input_data) >= 2) {
      
      # 데이터가 이미 데이터프레임이면 그대로 값을 확인 (desc22 등)
      if (is.data.frame(input_data)) {
        val12 <- input_data[1, 2]
      } else {
        # 특수 객체(cor22)인 경우에만 별도로 확인함
        check_mat <- matrix(input_data, nrow = nrow(input_data))
        val12 <- check_mat[1, 2]
      }
      
      # 1행 2열이 비어있거나(blank), NA인 경우 상관계수로 판단
      if (is.na(val12) || trimws(as.character(val12)) == "") {
        is_cor <- TRUE
      }
    }
  }, silent = TRUE)

  if (is_cor) {
    message("상관계수 행렬 ---- ")
    
    if (is.data.frame(input_data)) {
      df_cor <- input_data
    } else {
      raw_mat <- matrix(input_data, nrow = nrow(input_data), dimnames = dimnames(input_data))
      df_cor <- as.data.frame(raw_mat, stringsAsFactors = FALSE)
    }
    
    if (!"Variable" %in% colnames(df_cor)) {
      df_cor <- cbind(Variable = rownames(df_cor), df_cor)
    }
    rownames(df_cor) <- NULL
    
    print(df_cor)
    write_xlsx(df_cor, path = filenames)
    return(cat("\n[성공] 상관계수 파일 저장 완료:", filenames, "\n"))
  }
  # =========================================================================

  final_table <- NULL
  
  # 2. 클래스별 데이터 처리
  if (inherits(input_data, "multinom")) {
    message("다항 로짓(multinom) 결과 처리 -----")
    sum_obj <- summary(input_data)
    coef_matrix <- as.matrix(sum_obj$coefficients)
    se_matrix <- as.matrix(sum_obj$standard.errors)
    
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
    
    summary_info <- data.frame(
      Category = "Model Summary", Variable = c("Observations", "AIC", "BIC"),
      Estimate = c(nrow(input_data$fitted.values), round(AIC(input_data), 2), round(BIC(input_data), 2)),
      Std_Error = NA, t_z_value = NA, P_Value = NA, stringsAsFactors = FALSE
    )
    final_table <- rbind(final_table, summary_info)
    
  } else if (inherits(input_data, "glm")) {
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
