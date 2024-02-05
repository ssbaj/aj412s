e_subset<-function(explaining=0){

if(explaining==0) {
cat("  ", '\n')
cat("## subset 데이터 -------- ", '\n')
cat("  library(aj412s) ", '\n')
cat("    subset(BasicData, (brand=='KIA' | brand=='GM') , select=c(1,2,3,4)) ", '\n')
cat("    # select=c(1,2,3,4))는 1번 변수부터 4번 변수만 솎아내라는 명령문 ", '\n')
cat("    subset(BasicData, grepl('K', BasicData$brand) , select=c(1,2,3,4)) ", '\n')
cat("    # grepl은 brand변수의 레코드들 중 K가 포함된 모든 레코드를 솎아내라는 명령문입니다", '\n')
cat("   ", '\n')
cat("## subset 그래프 -------- ", '\n')
cat("  plot(subset(df, 평형==32, select=c(date0, 평당매매가평균)), type='l', col='cornflowerblue', lwd=2, ylim=c(1000, 2800)) ", '\n')
cat("   ", '\n')
cat("## subset 회귀분석 -------- ", '\n')
cat("  결과 <- lm(expend ~ inc + debt, data=df, subset=c(car_year==2021 & (dept==1 | dept==2) ) ) ", '\n')
cat("  결과 <- lm(expend ~ inc + debt, data=df, subset=c(car_year==2020 | car_year==2021) ) ", '\n')
cat("   ", '\n')

} }

