mlogit_prob <- function(mLogitResult, my_input=NULL) {

if (base::missing(mLogitResult)) {
cat(" \033[1;32m# 명령문 예제 ---------- \033[0m", '\n' )
cat(" \033[1;32m new_data <- data.frame( \033[0m", '\n' )
cat(" \033[1;32m gender = rep('남성', 61), \033[0m", '\n' )
cat(" \033[1;32m age = rep(30, 61), \033[0m", '\n' )
cat(" \033[1;32m income = seq(2000, 8000, 100) \033[0m", '\n' )
cat(" \033[1;32m ) \033[0m", '\n' )
return( cat("\033[1;33m 명령문: mlogit_prob(multinom분석결과, new_data) \033[0m", '\n') )
}

# 예측 확률 계산

probs <- predict(mLogitResult, newdata = my_input, type = "probs")

# 결과를 데이터프레임으로 변환
result <- as.data.frame(probs)

# 입력 데이터와 결과를 결합
cbind(my_input, result)
}
