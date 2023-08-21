e_ttest2<-function(explaining=0){
if(explaining==0) {
cat("  df<-BasicData  ", '\n')
cat("  jmv::ttestIS(  ", '\n')
cat("    formula = expend ~ gender,  # 양측검정 ", '\n')
cat("    data = df, # 데이터프레임명을 df로 지정  ", '\n')
cat("    welchs = TRUE,  ", '\n')
cat("    # norm = TRUE,  # normality 검정/Shapiro-Wilk검정  ", '\n')
cat("    eqv = TRUE,   # Levene’s Test  ", '\n')
cat("    desc = TRUE )", '\n')
}}
