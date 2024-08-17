# --- 상태전환 매트릭스 만들기
statetransition <- function(Adata){

	if (base::missing(Adata)) {
	cat("  \033[1;32m* NOTE: 소득분위 변화 테이블 \033[0m",'\n')
	cat("  \033[1;31m* 주의:  상태를 나타내는 코드값이 0일 경우 에러 발생 \033[0m",'\n')
	cat("  \033[1;31m* 주의:  상태 코드값은 반드시 1 이상으로 기록하세요 \033[0m",'\n')
	cat("    A0는 피조사자의 이름 ",'\n')
	cat("    A1은 2001년의 소득분위를 나타내는 column ",'\n')
	cat("    A2는 2002년의 소득분위를 나타내는 column ",'\n')
	cat("  \033[1;31m#------------------------------------ \033[0m",'\n')
	cat("    A0<-c('김준한','강명구','윤창근','김흥식','박성빈','김서용') ",'\n')
	cat("    A1<-c(1,2,1,3,1,4) ",'\n')
	cat("    A2<-c(1,2,2,3,3,3) ",'\n')
	cat("  \033[1;31m* 주의: df2는 A1과 A2처럼 2개의 변수만 포함하고, data.frame형태로, A1과 A2는 numeric으로 \033[0m",'\n')
	cat("    df<-as.data.frame(cbind(A0,A1,A2)) ",'\n')
	cat("    df2<-df[,c(2:3)] ",'\n')
	cat("  \033[1;31m* 주의: lapply 사용 후, 반드시 as.data.frame으로 data.frame형식을 지정해야 에러가 없음 \033[0m ",'\n')
	cat("    df2<-as.data.frame(df2)  ",'\n')
	cat("    tmp <- statetransition(df2)  ",'\n')
	cat("    tmp <- prop.table(as.matrix(tmp), margin=1)  ",'\n')
	cat("  \033[1;31m* 예: 2기간 후의 상태 ------------ \033[0m ",'\n')
	cat("    Mpower(tmp2, 2) ",'\n')
	return(cat("     "))  }
	
	if ( {sort(unique(Adata[,1]) )==0}[1]==T) {print('상태1 코드값은 1 이상의 값으로 기록하세요'); break}
	if ( {sort(unique(Adata[,2]) )==0}[1]==T) {print('상태2 코드값은 1 이상의 값으로 기록하세요'); break}
	
	Nrange1<-sum(unique(Adata[,1])>0)
	Nrange2<-sum(unique(Adata[,2])>0)
	
	if(Nrange1==Nrange2) {Nrange<-Nrange1}
	else if(Nrange1>Nrange2) {Nrange<-Nrange1}
	else {Nrange<-Nrange2}
	
    n<-nrow(Adata)
    ## Transition매트릭스 만들기
    Tmat <- matrix(0, nrow=Nrange, ncol=Nrange)
	Tmat<-as.data.frame(Tmat)

	for (i in 1:n) {
    Tmat[ Adata[,1][i], Adata[,2][i] ] <- Tmat[ Adata[,1][i], Adata[,2][i] ] + 1
    }
	Tmat<-as.data.frame(Tmat)
    return(Tmat)
}
