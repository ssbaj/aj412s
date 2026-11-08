statetransition <- function(x, y, option=1) {
  
if (missing(x)) {
    cat("  # 2023년 소득구간 (1=하, 2=중, 3=상), 500명", "\n")
    cat("  set.seed(123)", "\n")
    cat("  y2023 <- sample(1:3, 500, replace = TRUE, prob = c(0.3, 0.5, 0.2))", "\n")
    cat("  ", "\n")
    cat("  # 2024년 소득구간: 2023년 구간에 따라 이동 확률을 다르게 설정", "\n")
    cat("  p <- list(c(0.7, 0.25, 0.05),   # 1구간 -> 1, 2, 3", "\n")
    cat("            c(0.2, 0.6,  0.2),    # 2구간 -> 1, 2, 3", "\n")
    cat("            c(0.05, 0.25, 0.7))   # 3구간 -> 1, 2, 3", "\n")
    cat("  y2024 <- sapply(y2023, function(s) sample(1:3, 1, prob = p[[s]]))", "\n")
    cat("  ", "\n")
    cat("  # 전이 확률 행렬 / 빈도 행렬", "\n")
    cat("  trans_mat <- statetransition(y2023, y2024)             # option=1: 확률", "\n")
    cat("  statetransition(y2023, y2024, option = 2)              # option=2: 빈도", "\n")
    cat("  ", "\n")
    cat("  # 10년 뒤 분포 (Mpower 사용)", "\n")
    cat("  initial_dist <- c(0.3, 0.5, 0.2)", "\n")
    cat("  initial_dist %*% Mpower(trans_mat, 10)", "\n")
    return(cat("   \n"))
  }
  
  if (missing(y)) {
    stop("Error: y가 없음. statetransition(x, y)로 호출하시오.")
  }
  if (!option %in% c(1, 2) || length(option) != 1) {
    warning("option은 1(확률) 또는 2(빈도)만 가능함. option=1로 처리함.")
    option <- 1
  }

  # data.frame/list 등이 들어오면 벡터로 변환 (한 열짜리만 허용)
  if (is.data.frame(x)) x <- x[[1]]
  if (is.data.frame(y)) y <- y[[1]]
  x <- as.vector(unlist(x))
  y <- as.vector(unlist(y))

  if (length(x) != length(y)) {
    stop("Error: x와 y의 길이가 다르다.")
  }

  # NA가 있는 쌍은 제거
  ok <- !(is.na(x) | is.na(y))
  if (any(!ok)) {
    warning(sum(!ok), "개의 NA 포함 관측치를 제외함.")
  }
  x <- x[ok]
  y <- y[ok]
  if (length(x) == 0) {
    stop("Error: 유효한 (NA가 아닌) 관측치가 없음.")
  }

  # 정방행렬을 위한 모든 State 정의 및 Factor 변환
  all_states <- sort(unique(c(x, y)))
  x_factor <- factor(x, levels = all_states)
  y_factor <- factor(y, levels = all_states)

  tbl <- table(x_factor, y_factor)
  mat1_result <- matrix(as.numeric(tbl), nrow = nrow(tbl),
                        dimnames = dimnames(tbl))

  # 행 합계가 0인 상태(y에만 등장)는 0/0=NaN 대신 0으로 처리
  rs <- rowSums(mat1_result)
  mat2_result <- mat1_result
  nz <- rs > 0
  mat2_result[nz, ] <- mat1_result[nz, , drop = FALSE] / rs[nz]
  mat2_result[!nz, ] <- 0

  if (option == 2) return(mat1_result)
  return(mat2_result)
}
