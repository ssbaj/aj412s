e_ttest1<-function(explaining=0){
if(explaining==0) {
cat("  df<-BasicData  ", '\n')
cat("  jmv::ttestOneS(  ", '\n')
cat("     data = df, # 데이터프레임명을 Adata로 지정  ", '\n')
cat("     vars = expend,  ", '\n')
cat("     testValue = 300,  ", '\n')
cat("     # norm = TRUE,  # normality 검정/Shapiro-Wilk검정  ", '\n')
cat("     desc = TRUE )  ", '\n')
}}


