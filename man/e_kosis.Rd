\name{e_kosis}
\alias{e_kosis}
\title{How to load KOSIS data}
\usage{
e_kosis()
}
\description{
How to load KOSIS data
}
\examples{

##------------------------------------------------------  
     
# apkey 다운로드 받는 곳  
# https://kosis.kr/openapi/  
     
# KOSIS자료 Code Book  
# https://kosis.kr/openapi/devGuide/devGuide_0101List.do  
   
 ##------------------------------------------------------  
     
   library(kosis)   
     
   kosis.setKey(apiKey='당신의 API값을 입력하세요')    
     
#  MDE3ZDM1M2Y0MWIxELCA999NzI5OGIzZGExShimBIX777NmM1NWIzNTNlMjA=    
   
#  orgIds = 조직 ID  
#  prdSe = 수록주기: D(일), M, Q, H(3월), Y(년)  
  
 kosis데이터 <- getStatData(    
   orgId = '',   
   tblId = '',   
   prdSe = '',   
   itmId = '',   
   newEstPrdCnt = 숫자입력.따옴표 없음,  
   startPrdDe='',  
   endPrdDe='',  
   objL1='',  
   objL2='',  
   objL3=''  
 )  
 ## Data Format의 변경 --------------------  
   data<-data.frame(   
   v1<-c('서울', '서울', '서울','서울', '서울', '서울'),   
   v2<-c('노동','노동','노동', '광업','광업','광업'),   
   v3<-c(2023, 2022, 2021,2023, 2022, 2021),   
   v4<-c(33, 44, 55, 66, 88, 99)   
   )   
     
  ## R을 사용해 위 데이터셋을 다음과 같이 재배열하는 명령문 ---  
   data<-data.frame(   
   v1<-c('서울', '서울', '서울'),   
   연도<-c(2023, 2022, 2021),   
   노동<-c(33, 44, 55),   
   광업<-c(66, 88, 99)   
   )    
    
 ## ----------------------------------------  
   library(tidyr); library(dplyr)  
    
   # wide format으로 변환  
   data1 <- data %>% mutate(분류 = paste(C1_NM, C2_NM, sep = '_' ) ) 
   data2 <- data1 %>% select(분류, DT, PRD_DE)  
   data3 <- data2 %>% pivot_wider(   
      names_from = 분류,  
      values_from = DT  
   )    

}