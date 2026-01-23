\name{chk22}
\alias{chk22}
\title{Finding non-numeric cells}
\usage{
check_non_numeric()
}
\description{
Finding non-numeric cells
}
\examples{

  사용예: chk22(data)  #조사를 위한 dataset 지정  
  사용예: chk22(data, exclude=c(변수1, 변수2)) #따옴표 없이 변수명 입력 가능  
  사용예: chk22(data, exclude=c("V1", "V2")) #기존 방식(따옴표)도 가능  
  #------------------------------------  
           
  # numeric cell이 아닌 것을 모두 NA로 변경하기 
   data <- data %>%  
         mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.)))))  
}