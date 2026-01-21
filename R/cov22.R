cov22 <- function(x, y, use="complete.obs") {
  
  # 도움말 출력 (입력값이 없을 때)
  if (base::missing(x)) {
    cat("    ## Same command: cov(x, y, use='complete.obs') ", "\n")
    cat("    library(aj412s2); df<-BasicData", "\n")
    cat("    \033[1;31mcov22(x, y) \033[0m", "\n")
    return(cat("    \033[1;31mcov22(df$expend, df$inc) \033[0m"))  
  }

  c2n <- function(x){    
  if (base::missing(x)) {
    return(cat("  df$brandV2<-c2n(df$brand)"))
  }
  
  groups = unique(x)    
  groups = sort(groups)
  tmp <- as.numeric(factor(x, levels=groups))
  
  return(tmp)
}


  # ---------------------------------------------------
  # [수정된 부분] 문자형(character) 데이터 자동 변환 로직
  # ---------------------------------------------------
  
  # x가 문자형이면 c2n을 통해 변환
  if (is.character(x)) {
    cat("Note: 'x' is character. Converting to numeric using c2n()...\n")
    x <- c2n(x)
  }
  
  # y가 문자형이면 c2n을 통해 변환
  if (is.character(y)) {
    cat("Note: 'y' is character. Converting to numeric using c2n()...\n")
    y <- c2n(y)
  }
  
  # 공분산 계산 (기본값: complete.obs)
  cov_result <- cov(x, y, use = use)
  
  return(cov_result)
}