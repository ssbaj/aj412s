\name{e_ttest}
\alias{e_ttest}
\title{Calc: t-test of the group mean}
\usage{
e_ttest()
}
\description{
Calculating t-test of group mean
}
\examples{

# 가상의 데이터 생성 (학생 10명의 키)
heights <- c(172, 178, 176, 180, 171, 174, 179, 175, 173, 177)

# t-test 수행 (mu = 비교하려는 특정 값)
result <- t.test(heights, mu = 175)

# 결과 출력
print(result)

# 가상의 데이터 생성
group_A <- c(80, 85, 88, 90, 92)
group_B <- c(70, 75, 78, 80, 82)

# t-test 수행
t.test(group_A, group_B, var.equal = TRUE)
t.test(group_A, group_B, var.equal = FALSE)

# 예시 데이터 프레임
df <- data.frame(
  score = c(80, 85, 90, 70, 75, 78),
  class = c('A', 'A', 'A', 'B', 'B', 'B')
  )

# 공식(Formula) 방식 사용: 값 ~ 그룹
t.test(score ~ class, data = df, var.equal = TRUE)

}