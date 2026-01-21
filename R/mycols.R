#' mycols() Function
#' 
#' This function calculating colors
#' \033[1;36m# Examples ---------- \033[0m

mycols<-function(aa1, bb2, cc3){
if (base::missing(aa1)) {
    cat("  \033[1;31m  # ---------------------------------- ", '\n')
    cat("  \033[1;31m  mycols(Red비율, Green비율, Blue비율) ", '\n')
    cat("  \033[1;31m  mycols(0.3, 0.05, 0.3) ", '\n')
    cat("  \033[1;31m  # ---------------------------------- ", '\n')
    cat("  \033[1;32m  library(aj412s2) \033[0m ", '\n')
    cat("  \033[1;32m   \033[0m ", '\n')
    cat("  \033[1;32m  df<-data.frame( \033[0m ", '\n')
    cat("  \033[1;32m    index=c(1,2,3,4,5,6,7,8,9), \033[0m ", '\n')
    cat("  \033[1;32m    온도=c(10,11,13,14,17,19,20,24,21), \033[0m ", '\n')
    cat("  \033[1;32m    습도=c(40,45,50,51,39,49,54,57,63)  \033[0m ", '\n')
    cat("  \033[1;32m    ) \033[0m ", '\n')
    cat("  \033[1;32m   \033[0m ", '\n')
    cat("  \033[1;32m  mean(df$온도) \033[0m ", '\n')
    cat("  \033[1;32m  mean(df$습도) \033[0m ", '\n')
    cat("  \033[1;32m  mean(df$온도)/mean(df$습도) \033[0m ", '\n')
    cat("  \033[1;32m   \033[0m ", '\n')
    cat("  \033[1;32m  df$습도 <- df$습도 * ( mean(df$온도)/mean(df$습도) ) \033[0m ", '\n')
    cat("  \033[1;32m   \033[0m ", '\n')
    cat("  \033[1;32m  ggplot(df)+ \033[0m ", '\n')
    cat("  \033[1;32m    geom_line(aes(x = index, y = 온도, col = '온도'))+ \033[0m ", '\n')
    cat("  \033[1;32m    geom_line(aes(x = index, y = 습도,  col='습도')) +  \033[0m ", '\n')
    cat("  \033[1;32m    scale_y_continuous(sec.axis = sec_axis(~.*(1/0.3325893), name = '습도')) + \033[0m ", '\n')
    return( cat("  \033[1;32m    theme(axis.text.x = element_text(angle=30, hjust=1)) + theme_bw() \033[0m ", '\n') )  }
  mycols2 <- rgb(aa1, bb2, cc3, seq(0,1,length=10) )
  barplot(rep(1,10), space=0, axes=F, col=mycols2)
  return(mycols2)
}
