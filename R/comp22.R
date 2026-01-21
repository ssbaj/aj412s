comp22 <- function(dataset_name, ...) {
 
if (base::missing(dataset_name)) {
cat(" \033[1;36m# Examples ---------- \033[0m", '\n' )
cat(" \033[1;36mdf <- comp22(Adata, gender, debt) \033[0m", '\n' )
return( cat(" \033[1;36mdf <- comp22(Adata) \033[0m", '\n' )) }

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
