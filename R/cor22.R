cor22 <- function(x, y, digit = 2) {

    if (base::missing(x)) {
      cat("   ", "\n")
      cat("  \033[1;33mcor22(x, y, digit=4)\033[0m", "\n")
      cat("  \033[1;33mcor22(df, digit=4)\033[0m", "\n")
      return(cat("   \n"))
    }

  # 1. 문자형 -> 숫자형 변환 함수 (기존 유지)
  c2n <- function(x){    
    if (base::missing(x)) {
      return(cat("  Usage: df$col_new <- c2n(df$col_old)\n"))
    }
    groups = unique(x)    
    groups = sort(groups)
    tmp <- as.numeric(factor(x, levels=groups))
    return(tmp)
  }

  
  # ---------------------------------------------------
  # CASE 1: 데이터셋 입력 모드 (y가 없을 때)
  # ---------------------------------------------------
  if (missing(y)) {
    
    # 1. 데이터프레임 변환 및 전처리
    if (!is.data.frame(x)) x <- as.data.frame(x)
    
    # 문자형 변수 자동 변환 (c2n 적용)
    for (col_name in names(x)) {
      if (is.character(x[[col_name]])) {
        x[[col_name]] <- c2n(x[[col_name]])
      }
    }
    
    # 수치형 변수만 선택
    nums <- unlist(lapply(x, is.numeric))
    x_numeric <- x[, nums]
    
    # 2. 상관계수 계산 및 반올림
    res <- cor(x_numeric, use = "complete.obs")
    res <- round(res, digit)
    
    # 3. [추가된 기능] 상위 행렬 Blank 처리
    # 빈 문자열("")을 넣기 위해 행렬을 문자형으로 변환
    res_char <- as.matrix(res)
    
    # 대각선 기준 상위(Upper triangle)를 빈 값으로 변경
    res_char[upper.tri(res_char)] <- ""
    
    # 따옴표 없이 깔끔하게 출력하기 위해 noquote 객체로 반환
    return(noquote(res_char))
    
  } 
  
  # ---------------------------------------------------
  # CASE 2: 두 변수 입력 모드 (x, y가 있을 때)
  # ---------------------------------------------------
  else {
    if (is.character(x)) x <- c2n(x)
    if (is.character(y)) y <- c2n(y)
    
    res <- cor(x, y, use = "complete.obs")
    return(round(res, digit))
  }
}