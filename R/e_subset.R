e_subset<-function(explaining=0){

if(explaining==0) {
cat("  ", '\n')
cat("## subset 데이터 -------- ", '\n')
cat("  subset(df, 평형==32, select=c(date0, 평당매매가평균)) ", '\n')
cat("  subset(df, (brand=='KIA' | brand=='GM') ,select=c(1,2,3,4)) ", '\n')
cat("  subset(df, grepl('KI', df$brand) ,select=c(1,2,3,4)) ", '\n')
cat("   ", '\n')
cat("## subset 그래프 -------- ", '\n')
cat("  plot(subset(df, 평형==32, select=c(date0, 평당매매가평균)), type='l', col='cornflowerblue', lwd=2, ylim=c(1000, 2800)) ", '\n')
cat("   ", '\n')
cat("## subset 회귀분석 -------- ", '\n')
cat("  결과 <- lm(거래금액~전용면적+층, data=df, subset=c(year==2021 & 평형==32)  ) ", '\n')
cat("   ", '\n')

} }

