comp22 <- function(dataset_name, ...) {
 
if (base::missing(dataset_name)) {
cat(" \033[1;36m# Original 명령문 ---------- \033[0m", '\n' )
cat(" \033[1;35m  df<-df[complete.cases(df$conv), ]   \033[0m", '\n' )
cat(" \033[1;35m  \033[0m", '\n' )
cat(" \033[1;36m# del22 사용법 ---------- \033[0m", '\n' )
cat(" \033[1;35m  df <- comp22(Adata, gender, debt) \033[0m", '\n' )
return( cat(" \033[1;35m  df <- comp22(Adata) \033[0m", '\n' )) }

comp21<-function(Adata){
Adata <- Adata[complete.cases(Adata), ]
}

dataset_name <- deparse(substitute(dataset_name))
dataset <- get(dataset_name)

if(missing(...) ) { decision<-1 } else {decision<-0}

if(decision==1) {
cleaned_dataset<-comp21(dataset)
}

if(decision==0) {
var_names <- as.character(substitute(list(...)))[-1] # 첫 번째 항목은 "list"
cleaned_dataset <- dataset[complete.cases(dataset[var_names]), ]
}

return(cleaned_dataset)
}
