\name{e_dplyr}
\alias{e_dplyr}
\title{How to use dplyr}
\usage{
e_dplyr()
}
\description{
How to use dplyr
}
\examples{

## dplyr사용법 ------------------  
    
  df%>%group_by(브랜드) %>% summarize(편의성평균=mean(conv, na.rm=T), 표준편차=sd(conv, na.rm=T), 자료수=n())   
    
  df<-df%>%arrange(연령대)  
  df<-df%>%arrange(desc(연령대))  
    
  df<-merge(data1, data2, by='index') %>% merge(data3, by='index')  
       
  df<-df%>%relocate(브랜드, .before=성별)  
  df<-df%>%relocate(브랜드, .after=성별)  
    
  df%>%select(브랜드, 성별)  
## df$시군구 변수에서 레코드 솎아내기 ------------------  
  df%>%filter( grepl('용인수지구' , df$시군구))  
  df%>%filter( grepl('상현동', df$시군구) & grepl('상떼빌', df$단지명))  
  df%>%filter(변수E==2015 & (변수F=='C' | 변수F=='G') )  

  df<-rename(df, 새변수명=편의성) #rename대신 colnames(df)[컬럼번호]가 더 편리함 

}