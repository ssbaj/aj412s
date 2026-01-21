statetransition <- function(x, y, option=1) {
  
if (missing(x)) {
    cat("  trans_mat <- statetransition(x, y)", "\n")
    cat("  # 초기 분포 (2024년의 실제 비율, 예: 하30%, 중50%, 상20%)", "\n")
    cat("  initial_dist <- c(0.3, 0.5, 0.2) ", "\n")
    cat("  ", "\n")
    cat("  # Q: 10년 뒤에 소득 1, 2, 3구간에 사람들이 각각 몇 %씩 존재할까?", "\n")
    cat("  future_dist_10y <- initial_dist %*% Mpower(trans_mat, 10)", "\n")
    cat("  ", "\n")
    cat("  # Q: 지금 1구간인 사람이 정확히 10년 뒤에 2구간에 가 있을 확률은 얼마인가?", "\n")
    cat("  Mpower(trans_mat, 10) ", "\n")
    return(cat("   \n"))
  }
  
  if (length(x) != length(y)) {
    stop("Error: x와 y의 길이가 다릅니다.")
  }
  
  # 2. 정방행렬을 위한 모든 State 정의 및 Factor 변환
  all_states <- sort(unique(c(x, y)))
  x_factor <- factor(x, levels = all_states)
  y_factor <- factor(y, levels = all_states)
  
  # 3. 테이블 생성
  tbl <- table(x_factor, y_factor)
  prop_tbl <- prop.table(tbl, 1)
  
  # 4. [핵심 수정] 행렬로 변환 및 클래스 강제 지정
  # as.matrix()만 쓰면 간혹 "matrix" "array"로 잡힐 수 있으므로
  # 확실하게 "matrix"라고 명시합니다.
  mat1_result <- as.matrix(tbl)
  mat2_result <- as.matrix(prop_tbl)
  attr(mat1_result, "class") <- "matrix"
  attr(mat2_result, "class") <- "matrix"
  
  mat2_result<-as.data.frame(mat2_result)
  mat2_result[is.na(mat2_result)]<-0
  mat2_result<-as.matrix(mat2_result)

  if(option==2) {return(mat1_result)}
  if(option==1) {return(mat2_result)}
  
}