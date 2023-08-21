e_logit<-function(explaining=0){
if(explaining==0) {
cat("  library(gmodels) ", '\n')
cat("  re <- glm(debt2~car_conv+car_satprice+gender, family='binomial', data=df) ", '\n')
cat("  아무거나2<-c(1, 3, 4, 1) ", '\n')
cat("  아무거나3<-coef(re) ", '\n')
cat("  아무거나4<-sum(아무거나3*아무거나2) ", '\n')
cat("  Result_exp=exp(아무거나4) ", '\n')
cat("  부채보유_확률=Result_exp/(1+Result_exp)*100 ", '\n')
cat("  ... ", '\n')
cat("  또는, aj412s::logit_prob()를 사용   ", '\n')
cat("  Logit_prob(re, 아무거나2) ", '\n')
}}

