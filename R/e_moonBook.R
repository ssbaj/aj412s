#  How to use moonbook
e_moonBook<-function(explaining=0){
if(explaining==0) {
cat(" ", '\n')
cat("  library(moonBook); library(ztable); library(aj412s) ", '\n')
cat("  options(ztable.type='viewer')  ", '\n')
cat("  df<-BasicData  ", '\n')
cat("  ## 회귀분석결과를 테이블로 만들기 ", '\n')
cat("  re<-lm(car_satprice~gender+car_conv, data=df) ", '\n')
cat("  ztable(re) # 회귀분석 결과를 r의 viewer로 출력 -> 엑셀로 카피  -> 데이터 정돈 -> 워드/한글에 붙여넣기 ", '\n')
cat(" ", '\n')
cat("  re1<-mytable( ~gender+age+edu+expend+debt, data=df, max.ylev=5)   ", '\n')
cat("  re2<-mytable(gender~age+edu+expend+debt, data=df, max.ylev=5)   ", '\n')
cat("  re3<-mytable(gender~., data=df, max.ylev=5)   ", '\n')
cat("  ", '\n')
cat("  # 결과물을 viewer에 보여주는 기능  ", '\n')
cat("  ztable(re1); ztable(re2); ztable(re3); ", '\n')
cat("  ", '\n')

}}
