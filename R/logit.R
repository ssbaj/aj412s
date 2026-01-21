logit <- function(formula, data, subset) {
  # subset을 적용한 데이터 생성
  data_subset <- data[subset, ]
  # 로지스틱 회귀 모델 적합
  suppressPackageStartupMessages(library("gmodels"))
  RE_Logit <- glm(formula, data = data_subset, family = binomial(link='logit'))
  return(RE_Logit)
}

