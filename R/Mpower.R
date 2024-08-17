# 동일한 매트릭스를 반복적으로 곱하는 함수 만들기
Mpower <- function(mpower_data, n) {

if( max(mpower_data)>1 ) {
cat('  Dataset must be matrix, and its elements should be probabilities less than 1.0 ', '\n')
}

if (base::missing(mpower_data)) {
    return(cat("  Mpower(df_matrix, 3)  *NOTE: 3=multiply df-matrix 3 times"))  }

L <- list(mpower_data)
if (n==1) return(L)
P <- mpower_data
for(i in 2:n){L[[i]] <- (P <- P %*% mpower_data)}
return(L[[n]])
}
