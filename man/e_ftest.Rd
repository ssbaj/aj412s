\name{e_ftest}
\alias{e_ftest}
\title{Calc: One-Way Ftest}
\usage{
e_ftest()
}
\description{
Calculating One-Way Ftest
}
\examples{

# 데이터 생성
# 그룹별로 각각 평균이 10, 15, 20인 데이터를 생성
group_A <- c(10, 12, 11, 13, 10)
group_B <- c(14, 16, 15, 17, 15)
group_C <- c(20, 22, 19, 21, 20)

# 분석하기 좋게 데이터 프레임으로 합침
df <- data.frame(
  height = c(group_A, group_B, group_C),
  fertilizer = rep(c("A", "B", "C"), each = 5)
)

# 그룹 변수는 반드시 factor(범주형) 처리를 해주는 것이 편함
df$fertilizer <- as.factor(df$fertilizer)

# 데이터 확인
print(head(df))


# ANOVA 모델 생성
anova_result <- aov(height ~ fertilizer, data = df)

# 결과 요약 (F값과 p-value 확인)
summary(anova_result)

# 사후 검정 수행
TukeyHSD(anova_result)

# 기본 그래프
boxplot(height ~ fertilizer, data = df,
        main = "비료 종류별 식물 성장 비교",
        xlab = "비료 종류", ylab = "성장 높이",
        col = c("lightblue", "lightgreen", "pink"))

}