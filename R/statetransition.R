# --- 상태전환 매트릭스 만들기
statetransition <- function(Adata){
    
	if (base::missing(Adata)) {
	cat("  * NOTE: 소득분위 변화 테이블",'\n')
	cat("    A1은 2001년의 소득분위를 나타내는 column ",'\n')
	cat("    A2는 2002년의 소득분위를 나타내는 column ",'\n')
	cat("    A0<-c('김준일','강명일','윤창일','김흥이','박성삼','김서삼') ",'\n')
	cat("    A1<-c(1,2,1,3,1,4) ",'\n')
	cat("    A2<-c(1,2,2,3,3,3) ",'\n')
	cat("    df<-as.data.frame(cbind(A0,A1,A2)) ",'\n')
	cat("  * 주의: 분석에 사용되는 df2는 A1과 A2만 포함하고, data.frame형태이며, A1과 A2는 모두 numeric ",'\n')
	cat("    df2<-df[,c(2:3)] ",'\n')
	cat("    df2<-lapply(df2, as.numeric)  ",'\n')
	cat("    df2<-as.data.frame(df2)  ",'\n')
	cat("    tmp <- statetransition(df2)  ",'\n')
	return(cat("    round( prop.table(as.matrix(tmp), 1), 2)*100 "))  }
	
	
	Nrange1<-sum(unique(Adata[,1])>0)
	Nrange2<-sum(unique(Adata[,2])>0)
	
	if(Nrange1==Nrange2) {Nrange<-Nrange1}
	else if(Nrange1>Nrange2) {Nrange<-Nrange1}
	else {Nrange<-Nrange2}
	
    n<-nrow(Adata)
    ## Transition매트릭스 만들기
    Tmat <- matrix(0, nrow=Nrange, ncol=Nrange)
	for (i in 1:n) {
    Tmat[ Adata[,1][i], Adata[,2][i] ] <- Tmat[ Adata[,1][i], Adata[,2][i] ] + 1
    }
	Tmat<-as.data.frame(Tmat)
    return(Tmat)
}
