# short version of complete.cases

comp22<-function(Adata){
if (base::missing(dataset)) {
    cat("   \033[1;36m# 실제 명령문 ------------- \033[0m", '\n' )
	cat("   \033[1;36mAdata <- dataset[complete.cases(Adata$V1, Adata$V2), ] \033[0m", '\n' )
    return( cat("   \033[1;36mAdata <- dataset[complete.cases(Adata), ] \033[0m", '\n' )) }
Adata <- Adata[complete.cases(Adata), ]
}
