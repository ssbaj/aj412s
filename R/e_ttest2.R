e_ttest2<-function(explaining=0){
if(explaining==0) {
cat("    ", '\n')
cat("  jmv::ttestIS(  ", '\n')
cat("    formula = expend ~ gender,  # 양측검정 ", '\n')
cat("    data = df, # 내장데이터BasicData를 df로 저장  ", '\n')
cat("    welchs = TRUE,  ", '\n')
cat("    eqv = TRUE,   # Levene’s Test  ", '\n')
cat("    desc = TRUE )", '\n')
cat("    ", '\n')
}}
