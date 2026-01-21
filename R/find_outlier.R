find_outlier<-function(Result){
    if (base::missing(Result)) {
        return(cat("  find_outlier(회귀분석 결과 "))
    }

cat(" \033[1;36m# car패키지의 outlierTest() --------------- \033[0m", '\n'   )
print( car::outlierTest(Result) )

cat("    ", '\n')

cat(" \033[1;36m# car패키지의 influencePlot() --------------- \033[0m ", '\n'   )
print( car::influencePlot(Result) )
cat("    ", '\n')
cat(" \033[1;36m# 우측의 Plot은 influencePlot에서 제공한 Plot입니다\033[0m ", '\n'   )
cat("    ", '\n')
cat(" \033[1;36m# 다음은 Plot을 사용해 극단치를 찾는 방법입니다 \033[0m ", '\n'   )
plot( Result, which=c(1:5) )  # 극단치 찾아내기
cat("    ", '\n')

}