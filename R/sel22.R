sel22 <- function(dataset_name, ...) {

if (base::missing(dataset_name)) {
cat(" \033[1;36m# 명령문 예제 ---------- \033[0m", '\n' )
return( cat(" \033[1;36mdf <- sel22(Adata, 변수1, 변수2) \033[0m", '\n' )) }

if(missing(...) ) { stop(paste("Error: Variables are not litsed.")) }

# 데이터셋 이름을 문자열로 변환 후 실제 객체로 가져오기
dataset_name <- deparse(substitute(dataset_name))
dataset <- get(dataset_name)

# 선택된 변수 이름을 가져오기
var_names <- as.character(substitute(list(...)))[-1] # 첫 번째 항목은 "list"

# 새로운 데이터셋 생성
selected_dataset <- dataset[, var_names, drop = FALSE]

return(selected_dataset)
}
