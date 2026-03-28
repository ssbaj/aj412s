head22 <- function(df) {
  n <- nrow(df)
  
  # 각 지점의 시작 인덱스 계산
  idx_1st <- 1
  idx_q1  <- floor(n * 0.25)
  idx_q2  <- floor(n * 0.50)
  idx_q3  <- floor(n * 0.75)
  idx_last <- max(1, n - 4)
  
  # 구분선 행 생성 함수 (데이터 프레임의 컬럼 개수와 형식을 맞춤)
  make_sep <- function(label) {
    sep_row <- as.data.frame(matrix("---------", nrow = 1, ncol = ncol(df)))
    colnames(sep_row) <- colnames(df)
    rownames(sep_row) <- label
    return(sep_row)
  }
  
  # 각 구간 데이터 추출 (최대 5행)
  get_part <- function(start) {
    end <- min(start + 2, n)
    return(df[start:end, ])
  }
  
  # 전체 데이터를 구분선과 함께 결합
  # 행 이름을 유지하기 위해 rbind 사용 시 주의
  combined <- rbind(
    make_sep("START"),    get_part(idx_1st),
    make_sep("1/4 POINT"), get_part(idx_q1),
    make_sep("2/4 POINT"), get_part(idx_q2),
    make_sep("3/4 POINT"), get_part(idx_q3),
    make_sep("END"),       get_part(idx_last)
  )
  
  return(combined)
}
