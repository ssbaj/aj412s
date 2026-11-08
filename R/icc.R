## multilevel model's ICC (구 sig2 통합)

ICC<-function(re, signal=999) {
  if (base::missing(re)) {
    cat("  library(lme4)", "\n")
    cat("  ", "\n")
    cat("  # 1. 예제 데이터: 30개 학교, 학교당 20명 학생의 시험점수", "\n")
    cat("  set.seed(123)", "\n")
    cat("  n_school  <- 30", "\n")
    cat("  n_student <- 20", "\n")
    cat("  school <- factor(rep(1:n_school, each = n_student))", "\n")
    cat("  ", "\n")
    cat("  u <- rnorm(n_school, 0, 4)                 # 학교 효과 (분산 16)", "\n")
    cat("  e <- rnorm(n_school * n_student, 0, 8)     # 학생 오차 (분산 64)", "\n")
    cat("  score <- 70 + u[school] + e", "\n")
    cat("  ", "\n")
    cat("  dat <- data.frame(school, score)", "\n")
    cat("  ", "\n")
    cat("  # 2. 영모형(null model): 랜덤절편만 포함", "\n")
    cat("  re <- lmer(score ~ 1 + (1 | school), data = dat)", "\n")
    cat("  ", "\n")
    cat("  # 3. ICC 계산", "\n")
    cat("  ICC(re)                  # ICC만 출력", "\n")
    cat("  ICC(re, 1)               # VarCorr 표도 함께 출력", "\n")
    cat("  ", "\n")
    cat("  # 4. 분산성분과 ICC 값을 꺼내서 사용", "\n")
    cat("  res <- ICC(re)", "\n")
    cat("  res$sig2                 # 분산 벡터 (학교 간 분산, 잔차 분산)", "\n")
    cat("  res$ICC                  # ICC 값", "\n")
    cat("  ", "\n")
    cat("  # 설정한 참값은 16 / (16 + 64) = 0.20이므로, ICC가 0.2 근처로 나와야 합니다.", "\n")
    cat("  # ICC()는 (1|id) 형태의 영모형에서만 쓸 수 있습니다.", "\n")
    cat("  # (1+x|id)처럼 랜덤효과가 더 있는 모형은 VarCorr의 행 순서가 달라져서 값이 틀리게 나옵니다.", "\n")
    return(cat("   \n"))
  }

  tmp <- data.frame(VarCorr(re))
  sig <- tmp$vcov

  if (signal != 999) {
    print(tmp)
    cat(" ---------------------- ", '\n')
  }

  cat(" ", '\n')
  cat("\033[1;31m  # ICC for (1|id) -------------------\033[0m ", '\n')
  tmp_est <- sig[1]/(sig[1]+sig[2])
  cat("ICC =", tmp_est, '\n')

  return(invisible(list(sig2=sig, ICC=tmp_est)))
}
