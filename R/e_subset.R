e_subset<-function(explaining=0){

if(explaining==0) {
cat("  ", '\n')
cat("\033[1;31m## subset 데이터 -------- \033[0m", '\n')
cat("  library(aj412s) ", '\n')
cat("    subset(BasicData, (brand=='KIA' | brand=='GM') , select=c(1,2,3,4)) ", '\n')
cat("    # select=c(1,2,3,4))는 1번 변수부터 4번 변수만 솎아내라는 명령문 ", '\n')
cat("    subset(BasicData, grepl('K', BasicData$brand) , select=c(1,2,3,4)) ", '\n')
cat("    # grepl은 brand변수의 레코드들 중 K가 포함된 모든 레코드를 솎아내라는 명령문입니다", '\n')
cat("   ", '\n')
cat("\033[1;31m## subset 그래프 -------- \033[0m", '\n')
cat("   ", '\n')
cat("  plot(subset(df, 평형==32, select=c(date0, 평당매매가평균)), type='l', col='cornflowerblue', lwd=2, ylim=c(1000, 2800)) ", '\n')
cat("   ", '\n')
cat("\033[1;31m## subset으로 32평의 평당매매가평균을 하나의 그래프로 그리기 --------\033[0m ", '\n')
cat("  plot(subset(df, 평형==32, select=c(date0, 평당매매가평균)), type='l', ", '\n')
cat("       col='cornflowerblue', lwd=2, ylim=c(1000, 2800)) ", '\n')
cat("  par(new=T) ", '\n')
cat("   ", '\n')
cat("\033[1;31m## subset을 이용해 특정 레코드만 솎아내서 회귀분석하기 --------\033[0m ", '\n')
cat("  결과 <- lm(거래금액~전용면적+층, data=df, \033[1;93msubset=c(year==2021 & 평형==32)\033[0m  ) ", '\n')
cat("   ", '\n')

cat("\033[1;31m## subset 회귀분석 -------- \033[0m", '\n')
cat("  결과 <- lm(expend ~ inc + debt, data=df, subset=c(car_year==2021 & (dept==1 | dept==2) ) ) ", '\n')
cat("  결과 <- lm(expend ~ inc + debt, data=df, subset=c(car_year==2020 | car_year==2021) ) ", '\n')
cat("   ", '\n')

} }

