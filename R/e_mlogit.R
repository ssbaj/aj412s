e_mlogit<-function(explaining=0){
if(explaining==0) {
cat("## multinomal logit 분석 ------ ", '\n')
cat("  library(nnet); library(stargazer)  ", '\n')
cat("  re<-multinom(debt3~car_conv+car_satprice+age+gender, data=df, maxit = 2000) ", '\n')
cat("  stargazer(re, type='text') ", '\n')
cat(" ", '\n')
cat("## 확률계산 전단계 ------ ", '\n')
cat("  inputdata<-c(1, 3, 2, 30, 1)     # 1=상수항, 3=conv, 2=satprice, 30=age, 1=gender ", '\n')
cat("  y2<-sum(mcoef[1,1:5]*inputdata)  ", '\n')
cat("  y3<-sum(mcoef[2,1:5]*inputdata)  ", '\n')
cat("  y4<-sum(mcoef[3,1:5]*inputdata)  ", '\n')
cat(" ", '\n')
cat("## 확률계산 -------  ", '\n')
cat("  prob.부채없음 <- 1/(1+exp(y2)+exp(y3)+exp(y4))  ", '\n')
cat("  prob.담보부채 <- exp(y2)/(1+exp(y2)+exp(y3)+exp(y4))  ", '\n')
cat("  prob.신용부채 <- exp(y3)/(1+exp(y2)+exp(y3)+exp(y4))  ", '\n')
cat("  prob.담보및신용부채 <- exp(y4)/(1+exp(y2)+exp(y3)+exp(y4))  ", '\n')

}}

