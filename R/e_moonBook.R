#  How to use moonbook
e_moonBook<-function(explaining=0){
if(explaining==0) {
cat(" ", '\n')
cat("  library(moonBook); library(ztable); library(aj412s) ", '\n')
cat(" ", '\n')
cat("  options(ztable.type='viewer')  ", '\n')
cat(" ", '\n')
cat("  df<-BasicData  ", '\n')
cat("  \033[1;31m## 회귀분석결과를 테이블로 만들기 ------ \033[0m ", '\n')
cat("  re<-lm(car_satprice~gender+car_conv, data=df) ", '\n')
cat("  ztable(re) # 회귀분석 결과를 r의 viewer로 출력 -> notepad++  -> 엑셀로 카피 -> 워드/한글에 붙여넣기 ", '\n')
cat(" ", '\n')
cat("  \033[1;36m## 변수들의 기술통계 작성 ------ \033[0m ", '\n')
cat("  \033[1;31m## max.ylev=5이므로 변수의 레코드 종류가 5이하면 범주형변수, 5이상이면 연속형변수로 간주됩니다 \033[0m ", '\n')
cat("  \033[1;36m## 범주형변수는 Frequency가, 연속형변수는 평균과 표준편차가 리포팅됩니다 \033[0m ", '\n')
cat("  결과1<-mytable( ~gender+age+edu+expend+debt, data=df, max.ylev=5)   ", '\n')
cat(" ", '\n')
cat("  \033[1;36m## gender값으로 데이터를 나눈 후, 변수의 기술통계를  리포팅하기 ------ \033[0m ", '\n')
cat("  결과2<-mytable(gender~age+edu+expend+debt, data=df, max.ylev=5)   ", '\n')
cat(" ", '\n')
cat("  \033[1;36m## gender값으로 데이터를 나눈 후, 모든 변수의 기술통계를  리포팅하기 ------ \033[0m ", '\n')
cat("  결과33<-mytable(gender~., data=df, max.ylev=5)   ", '\n')
cat("  ", '\n')
cat("  # 결과물을 viewer에 보여주는 기능  ", '\n')
cat("  ztable(결과1); ztable(결과2); ztable(결과3); ", '\n')
cat("  ", '\n')

}}
