## multilevel model's ICC

icc<-function(modelxxxx) {
if (base::missing(modelxxxx)) {
    cat("  ICC는 (1|id)인 케이스에서만 사용 가능함  ", '\n')
	return( cat("  (1+var1+var2|id)인 케이스에는 사용 불가능 ", '\n') )
}

cat(" ", '\n')
  cat("\033[1;31m  # ICC for (1|id) -------------------\033[0m ", '\n')
  tmp<-data.frame( VarCorr(modelxxxx) )
  tmp_est <- tmp[1,4]/(tmp[1,4]+tmp[2,4])
cat("ICC =",tmp_est, '\n')
}
