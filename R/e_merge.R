e_merge<-function(explaining=0){

if(explaining==0) {
cat("  ", '\n')
cat("\033[1;31m## merge 데이터 -------- \033[0m", '\n')
cat("# 두 개의 데이터셋을 묶을 수 있는 id변수가 공통으로 존재해야 함", '\n')
cat("  ", '\n')
cat("  df1<-data.frame(  ", '\n')
cat("   id=c(1,2,3,4,5),  ", '\n')
cat("   age=c(23,45,32,NA,56),  ", '\n')
cat("   gender=c(1,2,1,1,2))  ", '\n')
cat("    ", '\n')
cat("  df2<-data.frame(  ", '\n')
cat("   id=c(1,2,3,4,5),  ", '\n')
cat("   income=c(2,3,4,3,5))  ", '\n')
cat("    ", '\n')
cat("  merge( df1, df2, by='id', all=T )  ", '\n')
cat("    id age gender income  ", '\n')
cat("  1  1  23      1      2  ", '\n')
cat("  2  2  45      2      3  ", '\n')
cat("  3  3  32      1      4  ", '\n')
cat("  4  4  NA      1      3  ", '\n')
cat("  5  5  56      2      5  ", '\n')
cat("   ", '\n')

} }

