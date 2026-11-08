scatmat<-function(... , nclass=NULL) {

if (base::missing(...)) {
	cat("  # NA미싱데이터가 있으면 실행불가능 ------------------------------- ", '\n')
	cat("  x축 변수는 그 칸과 같은 열(세로 방향)에 있는 대각선 변수  ", '\n')
	cat("  y축 변수는 그 칸과 같은 행(가로 방향)에 있는 대각선 변수  ", '\n')
	cat("  # tmp<-Adata[, c('conv','satprice','age','edu','nfamily')] ", '\n')
	cat("  # tmp<-tmp[complete.cases(tmp), ]  # NA미싱데이터가 있으면 실행불가능", '\n')
	return( cat("  scatmat(tmp) ", '\n') )
}

cat("  # ---------------------------------------------------------------------------- ", '\n')
cat("  x축 변수는 그 칸과 같은 열(세로 방향)에 있는 대각선 변수  ", '\n')
cat("  y축 변수는 그 칸과 같은 행(가로 방향)에 있는 대각선 변수  ", '\n')
cat("  # complete.cases명령문을 사용해 NA미싱데이터를 제거한 후, scatmat을 실행합니다 ", '\n')
cat("  tmp<-Adata[, c('conv','satprice','age','edu','nfamily')] ", '\n')
cat("  tmp<-tmp[complete.cases(tmp), ]  # NA미싱데이터가 있으면 실행불가능", '\n')
cat("  scatmat(tmp) ", '\n')
cat("  viewer의 그래프를 보면, 대각선 변수가 x축 변수, 교차되는 변수가 y축 변수 ", '\n')
cat("  # ---------------------------------------------------------------------------- ", '\n')

  dat <- cbind(...)
  vnames <- colnames(dat)
  if (is.null(vnames)) vnames <- paste0("V", seq_len(ncol(dat)))
  pairs( dat ,
         panel=function(x,y){
           points(x,y)
           abline(lm(y~x), lty=2)
           lines(lowess(x,y))
           pos <- par("mfg")   # 현재 칸의 (행, 열) 위치
           mtext(vnames[pos[2]], side=1, line=-1, cex=0.7, col="blue")  # x축 변수명
           mtext(vnames[pos[1]], side=2, line=-1, cex=0.7, col="red")   # y축 변수명
         },
         diag.panel=function(x){
           par(new=T)
           hist(x, main="", axes=F, nclass=nclass)
         }
      )
  }


