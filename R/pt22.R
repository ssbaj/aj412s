pt22 <- function(t, df) {
  # t : t-통계량 (t-value)
  # df: 자유도 (degrees of freedom)

  if (base::missing(t)) {
            cat("    ", "\n")
	    cat("   \033[1;31mpt22(t값, df자유도) \033[0m", "\n")
	    return(cat("   \033[1;31mpt22(-1.960, 9999) \033[0m"))  }
  
  # 절대값을 사용하여 양측 검정(two-tailed) p-value 계산
  p_value <- 2 * pt(-abs(t), df)
  
  return(p_value)
}