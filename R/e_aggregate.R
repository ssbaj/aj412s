e_aggregate<-function(explaining=0){

if(explaining==0) {
cat("  ", '\n')
cat("\033[1;31m## aggregate 사용법 -------- \033[0m", '\n')
cat("  # aggregate : 종속변수의 값을 독립변수 값에 따라 집계한 후, ", '\n')
cat("  #             평균mean, 자료수length, 중위수median 등의 함수 값을 출력한다", '\n')
cat("  #             사용예: aggregate(종속변수~독립변수, 데이터셋, 함수mean)", '\n')
cat("    ", '\n')
cat("  df<-data.frame(  ", '\n')
cat("   area=c(1,2,3,1,2,3),  ", '\n')
cat("   age=c(23,45,32,21,56,76),  ", '\n')
cat("   gender=c(1,2,1,1,2,1))  ", '\n')
cat("    ", '\n')
cat("   aggregate(age~area, df, mean) ", '\n')
cat("     area  age ", '\n')
cat("   1    1 22.0 ", '\n')
cat("   2    2 50.5 ", '\n')
cat("   3    3 54.0 ", '\n')
cat("   ", '\n')

} }

