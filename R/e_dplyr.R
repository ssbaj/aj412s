e_dplyr<-function(explaining=0){

if(explaining==0) {
cat("  ", '\n')
cat("## dplyr사용법 ------------------ ", '\n')
cat("   ", '\n')
cat("  df %>% group_by(브랜드) %>% summarize(편의성평균=mean(conv, na.rm=T),   ", '\n')
cat("                                      자료수=n(), .groups='drop_last')  ", '\n')
cat("   ", '\n')
cat("  df<-df %>% arrange(연령대) ", '\n')
cat("  df<-df %>% arrange(desc(연령대)) ", '\n')
cat("   ", '\n')
cat("  df<-merge(data1, data2, by='index') %>% merge(data3, by='index') ", '\n')
cat("      ", '\n')
cat("  df<-df %>% relocate(브랜드, .before=성별) ", '\n')
cat("  df<-df %>% relocate(브랜드, .after=성별) ", '\n')
cat("   ", '\n')
cat("  df %>% select(브랜드, 성별) ", '\n')
cat("   ", '\n')
cat("## df$시군구 변수에서 레코드 솎아내기 ------------------ ", '\n')
cat("  df %>% filter( grepl('용인수지구' , df$시군구)) ", '\n')
cat("  df %>% filter( grepl('상현동', df$시군구) & grepl('상떼빌', df$단지명)) ", '\n')
cat("   ", '\n')
cat("  df %>% filter(변수E==2015 & (변수F=='C' | 변수F=='G') ) ", '\n')
cat("   ", '\n')
cat("  df<-rename(df, 새변수명=편의성) #rename대신 colnames(df)[컬럼번호]가 더 편리함", '\n')
cat("   ", '\n')
} }

