e_string<-function(explaining=0){
if(explaining==0) {
cat("\033[1;31m## string문자열 예제 명령문 ------  \033[0m", '\n')
cat("   ", '\n')
cat("  v1<-c(1,2,3) ", '\n')
cat("  v2<-c('경기도 일산구', '경기도 수지구', '경기도 성남시') ", '\n')
cat("  df<-data.frame(v1, v2) ", '\n')
cat("   ", '\n')
cat("\033[1;31m## df 데이터셋 ------  \033[0m", '\n')
cat("  > head(df) ", '\n')
cat("   v1            v2 ", '\n')
cat("  1 경기도 일산구 ", '\n')
cat("  2 경기도 수지구 ", '\n')
cat("  3 경기도 성남시 ", '\n')
cat("   ", '\n')
cat("  for(i in 1:3) { ", '\n')
cat("\033[1;31m# nchar \033[0m", '\n')
cat("  n<-nchar(df$v2[i]) ", '\n')
cat("\033[1;31m# substring \033[0m", '\n')
cat("  print(substring(df$v2[i], n)) ", '\n')
cat("\033[1;31m# gsub \033[0m", '\n')
cat("  Ex2<-gsub('경기도', 'GG', df$v2[i]) ", '\n')
cat("  print(Ex2) ", '\n')
cat("   ", '\n')
cat("   ", '\n')

}  }

