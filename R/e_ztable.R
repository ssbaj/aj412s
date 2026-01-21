#  How to use moonbook and ztable
e_ztable<-function(explaining=0){
if(explaining==0) {
options(ztable.type='viewer')
cat("  library(moonBook); library(ztable) ", '\n')
cat("  ", '\n')
cat("  options(ztable.type='viewer')  ", '\n')
cat("  ", '\n')
cat("  # max.ylev는 독립변수 코딩값이 몇 종류인지를 지정하는 옵션. 디폴트=5  ", '\n')
cat("  # 코딩값의 종류가 max.ylev에서 지정한 숫자를 넘어서면 그 변수는 연속형변수로 인식  ", '\n')
cat("  # 코딩값의 종류가 max.ylev에서 지정한 숫자 이하면 그 변수는 범주형변수로 인식  ", '\n')
cat(" ", '\n')
cat("  ## 변수의 기술통계 ----- ", '\n')
cat("  mytable(~conv+satprice, data=df) ", '\n')
cat("  mytable(brand~expend+age, data=df, max.ylev=4) ", '\n')
cat(" ", '\n')
cat("  ## 회귀분석결과를 테이블로 만들기 ----- ", '\n')
cat("  분석결과<-lm(satprice ~ gender + conv, data=Adata) ", '\n')
cat("  ztable(분석결과, caption='Table 13', caption.position='l', zebra=1)  ", '\n')
cat("  ", '\n')
}}
