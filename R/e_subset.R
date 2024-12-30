e_subset<-function(explaining=0){

if(explaining==0) {
cat("  ", '\n')
cat("\033[1;31m## subset 데이터 -------- \033[0m", '\n')
cat("  subset( BasicData, debt>=29 ) ", '\n')
cat("    # BasicData의 debt변수값이 29 이상인 것만 솎아낸다 ", '\n')
cat("  ", '\n')
cat("  subset( BasicData, (brand=='KIA' | brand=='GM') ) ", '\n')
cat("    # (brand=='KIA' | brand=='GM') : brand의 값이 KIA와 GM데이터만 솎아낸다 ", '\n')
cat("  ", '\n')
cat("  subset( BasicData, select=c(1,2,3,4) ) ", '\n')
cat("    # select=c(1,2,3,4) : 1번 변수부터 4번 변수만 솎아내라는 명령문 ", '\n')
cat("  ", '\n')
cat("  subset(BasicData, grepl('K', BasicData$brand) , select=c(1,2,3,4)) ", '\n')
cat("    # grepl은 brand변수의 레코드들 중 K가 포함된 모든 레코드를 솎아내라는 명령문입니다", '\n')
cat("   ", '\n')
cat("\033[1;31m## subset 회귀분석 -------- \033[0m", '\n')
cat("  결과 <- lm(expend ~ inc + debt, data=df, \033[1;93msubset=c(car_year==2021 & (dept==1 | dept==2) )\033[0m )  ) ", '\n')
cat("  결과 <- lm(expend ~ inc + debt, data=df, \033[1;93msubset=c(car_year==2020 | car_year==2021)\033[0m )  ) ", '\n')
cat("   ", '\n')

} }

