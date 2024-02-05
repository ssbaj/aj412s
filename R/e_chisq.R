e_chisq<-function(explaining=0){
if(explaining==0) {
cat("  library(gmodels)", '\n')
cat("    df<-Data30_Chisq ", '\n')
cat("    CrossTable(df$gender, df$smoking, chisq=TRUE)  ", '\n')
cat("  # ----------- ", '\n')
cat("  # 셀자료수 ", '\n')
cat("  # 카이자승값 ", '\n')
cat("  # 셀자료수/가로합", '\n')
cat("  # 셀자료수/세로합", '\n')
cat("  # 셀자료수/총자료수", '\n')
}}
