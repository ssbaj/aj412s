## 활용법: ols_pred(OLS분석결과)

ols_pred <- function( OlsResult, my_input=NULL ){

  if (is.null(my_input)) {
    cat("*** my_input으로 각 변수들의 레코드 값을 지정하지 않을 경우, 기본값을 사용함 ", '\n')
	cat("*** 연속형 변수는 20% 증가했을 경우를, 더미변수는 0->1로 되었을 경우의 예측치입니다", '\n')
	cat("*** my_input으로 각 변수들의 레코드 값을 지정해서 예측치를 얻는 방법:  ", '\n')
	cat("*** my_input<-c(1(상수항), 독립변수1의 값, 독립변수2의 값 ... 독립변수값을 지정)", '\n')
	cat("*** lm_pred(분석결과, my_input)  ", '\n')
    my_input <- 20
  }


if (base::missing(OlsResult)) {
	cat("  더미는 1증가, 연속변수 20%증가: ols_pred( OlsResult)   ", '\n')
	cat("  더미는 1증가, 연속변수 30%증가: ols_pred( OlsResult, 30)   ", '\n')
	cat("  ------------------------------------------------------------   ", '\n')
	cat("  독립변수에 임의의 값 지정: my_input<-c(1,0,2)  *NOTE: 1=constant, 0='value of X1', 2='value of X2' ", '\n')
	cat("  ols_pred( OlsResult, my_input)   ", '\n')
	return(cat("  ") ) }
	

## Start of Return Variable Names ----------------------------
Return_variablenames<-function(OlsResult) {
   tmpword<-OlsResult$terms
   count_tmpword<-nchar(tmpword)
   count_tmpword<-count_tmpword[3]
   
   tmpword<-as.character(tmpword)
   tmpword<-tmpword[3]
   tmpdataframe<-rep(NA, 100)  # 변수의 최대 갯수를 100개로 지정
   tmpdataframe<-as.data.frame(tmpdataframe)
   colnames(tmpdataframe)<-'vnames'
   
   tmp_count_check<-0
   tmp_count_origin<-1
   tmp_count<-0
   
   for(i in 1:count_tmpword){
   tmp_w<-substring(tmpword, i, i)
   
   if(tmp_w != "+") {
   tmp_count<-tmp_count+1 
    }
   
   if(tmp_w == "+") { 
   temp_word<-substring(tmpword, tmp_count_origin, (i-1) )
   temp_word<-gsub(" ", "", temp_word)
   tmp_count_check <- tmp_count_check+1
   tmpdataframe[tmp_count_check,1]<-temp_word
   tmp_count_origin<-i+1
   }
   
   temp_word<-substring(tmpword, tmp_count_origin, count_tmpword)
   tmpdataframe[(tmp_count_check+1),1]<-temp_word
   
   }

  tmpdataframe<-tmpdataframe[complete.cases(tmpdataframe), ]
  return(tmpdataframe)
}
## End of Return Variable Names ----------------------------


r=c()
max.ylev<-2
nvariables<-OlsResult$rank
tmp_r=c()

## 상수항 및 중위수값을 r에 대입. 첫 번째 항에 1을 대입
for(i in 1:nvariables){
if( ( nrow(unique(OlsResult$model[i])) > max.ylev )  ) { r=c(r, median(OlsResult$model[, i])) }
else {r<-c(r, 0)} }
r[1]<-1  ## 첫 번째 데이터는 종속변수의 중위수. 상수항 1로 교체한다


if( length(my_input)==1 ) { OriginData <- r } else{ OriginData<-my_input } 


cat('*** Origin데이터: 더미변수=0, 연속형변수=중위수 => c(', OriginData, ')','\n') 
cat('    Origin예측치를 위한 첫 번째 입력값 1은 상수항을 의미함', '\n') 
cat('*** 예측치계산1) 연속형변수값을 20% 증가시켰을 때의 예측치계산: ', 'ols_pred( OLS결과, 20 ) ', '\n')
cat('*** 예측치계산2) 변수값 지정한 후 예측치 => ', 'ols_pred(OLS결과, c(1, 독립변수1값, 독립변수2값 ...) ) ', '\n')
cat('*** Return <-ols_pred(OLS결과, c(1, 독립변수1값, 독립변수2값...)) 최종예측치가 return값으로 전달됨 ', '\n')
cat('*** ------------------------------------------------------------------------------    ', '\n')
if(length(my_input)==1) {  cat('   ','독립변수', '  Origin예측치   ', ' 증가=최종예측치- ', ' 최종예측치 ', '\n')  
						   cat('   ','            ', '            ', '    Origin예측치 ', '  ', '\n')  
						   cat('*** ------------------------------------------------------------------------------    ', '\n')
						}


variable_names<-Return_variablenames(OlsResult)

for(i in 2:length(OriginData)){
   input<-OriginData

if( length(my_input)==1 ) { OriginData <- r } else { OriginData<-my_input } 
if( is.null(my_input) ) { OriginData <- r }

## 각 열에서 unique한 숫자들이 몇 개 있는지 확인하는 절차
## 각 열에서 unique한 숫자가 2개이면 더미변수이므로 퍼센트 증가 대신, 0->1로 변경함
if( ( length(my_input)==1 ) & ( nrow(unique(OlsResult$model[i])) > max.ylev ) )  {
	input[i] <- (median(OlsResult$model[,i]))*(1 + my_input/100)  
	tmp_r<-c(tmp_r, input[i])}
    else {input[i]<-input[i]+1
		  tmp_r<-c(tmp_r, input[i])
		  }

   ### 입력된 원자료에 20%를 더한 자료 --------
   예측치계산자료<-as.data.frame(OlsResult$coef)
   예측치계산자료<-cbind(예측치계산자료, input)
   colnames(예측치계산자료)[1]<-c('Result')
   예측치계산자료$temp<-예측치계산자료$Result * 예측치계산자료$input
   tmp<-sum( 예측치계산자료[ ,3] )
   예측치<-round( tmp, 4)

  
   ### 입력된 원자료 --------
   예측치계산자료se<-as.data.frame(OlsResult$coef)
   예측치계산자료se<-cbind(예측치계산자료se, OriginData)
   colnames(예측치계산자료se)[1]<-c('Result')
   예측치계산자료se$temp <- 예측치계산자료se$Result*예측치계산자료se$OriginData
   tmp<-sum( 예측치계산자료se[ ,3] )
   예측치se<-round( tmp, 4)


   if(length(my_input)>1) {
   m_count<-length(my_input)
   variable_names2<-c('Intercept', variable_names)
   for( m in 1:m_count) {
   cat(  variable_names2[m] , "=" , my_input[m]  , "; ")
   }
   cat('    ' , '\n')
   cat('*** 예측치:', 예측치se, '\n')
   cat('*** ------------------------------------------------------------------------------ ', '\n')
   break
   }


   ChPROB<-round((예측치-예측치se) , 4)
   
   cat('    ', variable_names[i-1] )
   cat('    ', 예측치se)
   cat('        ', ChPROB)
   cat('        ', 예측치, '\n')

  }

if(length(my_input)==1) {cat('     ------------------------------------------------------------------------------ ', '\n') }
if(length(my_input)==1) {cat('     초기예측치 계산 INPUT자료', r, '\n') }
if(length(my_input)==1) {cat('     증가예측치 계산 INPUT자료', '#', tmp_r, '\n') }
if(length(my_input)>1) {return(예측치se) }

  }


