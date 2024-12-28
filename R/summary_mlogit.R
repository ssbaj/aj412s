summary_mlogit <- function(model) {

if (base::missing(model)) {
cat(" \033[1;36m# 명령문 예제 ---------- \033[0m", '\n' )
return( cat("\033[1;36m 명령문: summary_logit(multinom분석결과) \033[0m", '\n') )
}

suppressPackageStartupMessages(library("dplyr"))

# 계수, 표준오차, z값, p값 계산
coef_matrix <- coef(model)
se_matrix <- summary(model)$standard.errors
z_matrix <- coef_matrix / se_matrix
p_matrix <- (1 - pnorm(abs(z_matrix), 0, 1)) * 2

# 각 종속변수 범주에 대한 결과 출력
for (i in 1:nrow(coef_matrix)) {
category <- rownames(coef_matrix)[i]
cat("\nResults for category:", category, "\n")
results <- data.frame(
Coefficient = coef_matrix[i,],
Std.Error = se_matrix[i,],
z_value = z_matrix[i,],
p_value = p_matrix[i,]
) %>%
mutate(across(where(is.numeric), ~round(., 4)))
print(results)
}

# 모델 요약 정보 출력
cat("\n<< Model Summary >>\n")
cat(" Number of observations:", nrow(model$fitted.values), "\n")
cat(" AIC:", AIC(model), "\n")
}
