sd22 <- function(x, na.rm = TRUE) {
  
  # 1. 입력값이 없을 때 도움말 출력
  if (base::missing(x)) {
    cat("    sd22(x) : 표준편차 계산 (Character는 숫자로 자동 변환) \n")
    cat("    \033[1;31msd22(df$variable) \033[0m", "\n")
    return(cat("   ", "\n"))
  }

# (참고) c2n 함수가 먼저 정의되어 있어야 합니다.
c2n <- function(x){    
  if (base::missing(x)) {
    return(cat("  df$brandV2<-c2n(df$brand)"))
  }
  groups = unique(x)    
  groups = sort(groups)
  tmp <- as.numeric(factor(x, levels=groups))
  return(tmp)
}


  # 2. 문자형(character)인지 확인 후 변환
  if (is.character(x)) {
    cat("Note: sd22 input is character. Converting to numeric using c2n()...\n")
    x <- c2n(x)
  }

  # 3. 표준편차 계산 (기본적으로 NA 제거)
  sd_val <- sd(x, na.rm = na.rm)
  
  return(sd_val)
}