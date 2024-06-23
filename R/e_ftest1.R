e_ftest1<-function(explaining=0){
if(explaining==0) {
cat("     ", '\n')
cat("jmv::anovaOneW(     ", '\n')
cat("  formula = satprice ~ brand, ", '\n')
cat("  data = df, ", '\n')
cat("  welchs = TRUE,  # Levene’s Test결과 이분산이면 welchs=TRUE, fishers=FALSE ", '\n')
cat("  fishers = TRUE,  # Levene’s Test결과 등분산이면 welchs=FALSE, fishers=TRUE   ", '\n')
cat("  desc = TRUE,   ", '\n')
cat("  eqv = TRUE,  # Levene’s Test를 실행   ", '\n')
cat(" #phMethod = 'gamesHowell')  # Levene’s Test결과 이분산이면 gamesHowell검정 실행   ", '\n')
cat("  phMethod = 'tukey')  # Levene’s Test결과 등분산이면 tukey검정 실행   ", '\n')
cat("     ", '\n')
}}
