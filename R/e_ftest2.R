e_ftest2<-function(explaining=0){
if(explaining==0) {
cat("library(jmv)", '\n')
cat(" ", '\n')
cat("# Two-Way ANOVA ", '\n')
cat("# with(데이터셋, interaction.plot(독립변수1, 독립변수2, 종속변수, fun = mean, main = 'Interaction Plot'))", '\n')
cat("  with(df, interaction.plot(  gender, edu, satprice, fun = mean, main = 'Interaction Plot'))", '\n')
cat("  df$gender<-as.factor(df$gender)", '\n')  # gender변수를 범주형변수로 지정
cat("  df$brand<-as.factor(df$brand)", '\n')  # dept변수를 범주형변수로 지정
cat(" ", '\n')
cat("jmv::ANOVA(formula = satprice ~ gender + brand + gender:brand,  ", '\n')
cat("   data = df,", '\n')
cat("   effectSize = 'eta',", '\n')
cat("   homo = TRUE,  #Levene’s Test를 실행하란 명령문", '\n')
cat("   postHoc = ~ gender + brand + gender:brand,  ", '\n')
cat("   postHocCorr='tukey')  # Tukey방법의 PostHoc Test  ", '\n')
cat(" ", '\n')
cat("**** 다른 방법으로 PostHoc Test를 하려면 **** ", '\n')
cat("--- Scheffe방법의 PostHoc Test --- ", '\n')
cat("    postHocCorr = 'scheffe' ", '\n')
cat("--- Bonferroni방법의 PostHoc Test --- ", '\n')
cat("    postHocCorr = 'bonf'  ", '\n')
cat("--- Holm방법의 PostHoc Test ---", '\n')
cat("    postHocCorr = 'holm'  ", '\n')
}}

