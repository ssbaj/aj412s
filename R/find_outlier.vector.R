find_outlier.vector <- function(model_outlier, list_no=6, threshold=2){
  
 if (base::missing(model_outlier)) {
        return(cat("  find_outlier.vector(df$변수명 또는 vector, 리포팅되는 극단치 갯수(디폴트6개), qnorm(확률)=2) "))
    }
    model_outlier<-as.vector(model_outlier)
    n <- length(model_outlier)
    tmp <- rep(NA, n)
    dataset1 <- as.data.frame(tmp)
    colnames(dataset1)[1] <- "index"
    for (i in 1:n) {
        dataset1$index[i] <- i
    }
    dataset1$tmp_z1 <- scale(model_outlier)
    dataset1$tmp_z2 <- abs(scale(model_outlier))
    r1 = c()
    r11 = c()
    for (i in 1:n) {
        if (is.na(dataset1$tmp_z2[i])) {
            next
        }
        if (dataset1$tmp_z2[i] > threshold) {
            r1 = c(r1, dataset1[i, 1])
        }
        if (dataset1$tmp_z2[i] > threshold) {
            r11 = c(r11, dataset1[i, 3])
        }
    }
    library(dplyr)
    dataset1_arrange <- rbind(r1, r11)
    dataset1_arrange <- as.data.frame(t(dataset1_arrange))
    dataset1_arrange <- dataset1_arrange %>% arrange(desc(r11))
    cat("# \033[1;32m-----------------------------  \033[0m",
        "\n")
    cat("\033[1;31m  사용법: find_outlier(분석결과, 리포팅되는 극단치 갯수(디폴트6개), threshold=qnorm(확률) ) \033[0m",
        "\n")
    cat("\033[1;32m  절대값을 취한 |오차의 z값|이 threshold보다 큰 레코드가 Outlier \033[0m",
        "\n")
    cat("\033[1;32m  절대값을 취한 |오차의 z값|을 나누는 threshold값: \033[0m",
        threshold, "\n")
    cat("\033[1;32m  절대값을 취한 |오차의 z값|이 threshold보다 큰 레코드의 갯수: \033[0m",
        nrow(dataset1_arrange), "\n")
    colnames(dataset1_arrange)[1] <- c("Index")
    colnames(dataset1_arrange)[2] <- c("|z|")
    cat("\033[1;32m  Outlier를 절대값을 취해 큰 순서대로 나열하면 ------  \033[0m",
        "\n")
    colnames(dataset1_arrange)[1] <- c("Index")
    colnames(dataset1_arrange)[2] <- c("|z|")
    head(dataset1_arrange, list_no)
}

