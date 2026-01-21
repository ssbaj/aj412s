pz22 <- function(z) {
  # z : z-통계량 (z-value)
 
  if (base::missing(z)) {
            cat("    ", "\n")
	    cat("   \033[1;31mpz22(z값) \033[0m", "\n")
	    return(cat("   \033[1;31mpz22(-1.960) \033[0m"))  }
 
  
  # 절대값을 사용하여 양측 검정(two-tailed) p-value 계산
  # 표준정규분포(mean=0, sd=1) 기준입니다.
  p_value <- 2 * pnorm(-abs(z))
  
  return(p_value)
}
