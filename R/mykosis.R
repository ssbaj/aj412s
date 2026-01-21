# Adata의 변수는 3개: 1번=지역 소방서, 2번 변수=화재원인, 3번 변수=데이터
# 데이터 : https://data.seoul.go.kr/dataList/10949/S/2/datasetView.do
# 원본자료: Data45_KOSIS.xlsx
# V1:지역명(예, 강북소방서), V2:변수명(예, 담배꽁초), V3:데이터값
# 원본자료: Data45_KOSIS_Industry.xlsx
# V1:지역명(예, 전국, 서울, 경기도...), V2:변수명(예, 전체산업, 제조업, 건설업...), V3~V4:데이터값(예, 사업체수, 종사자수)
# 
# df<-read_excel('Data45_KOSIS_Industry.xlsx')
# 내장데이터명: KosisData
# df <- KosisData
# 
# 원자료에서 V1(지역명)이 NA로 표기된 것은 앞의 레코드로 채워넣는다
# na.locf = NA Last Observation Carried Forward
# df$V1<-zoo::na.locf(df$V1)
# NewData<-mykosis(df)
# ------------------------------------------------------

mykosis<-function(KosisDataSet){
## V1:지역명(예, 강북소방서), V2:변수명(예, 담배꽁초), V3~V4:데이터값

if (base::missing(KosisDataSet)) {
    cat("  df$V1<-zoo::na.locf(df$V1) ", "\n")
	cat("  NOTE: df = V1:지역명(예, 전국), V2:변수명(예, 제조업), V3:기업체수, V4:종사자수(명) ", "\n")
	cat("  df2<-df[,c(1,2,3)]  or  df2<-df[,c(1,2,4)]   ", "\n")
    return(cat("  tmp<-mykosis(df2) "))  }


colnames(KosisDataSet)<-c('V1', 'V2', 'V3')

# 원본자료의 갯수
n<-nrow(KosisDataSet)

## j: 지역명의 갯수
j<-length(unique( KosisDataSet[complete.cases(KosisDataSet[1]),][,1]))

## V1Temp: 지역명 종류, V1Number: 지역명의 갯수
V1Temp<-unique( KosisDataSet[complete.cases(KosisDataSet[1]),][,1])
V1Number<-sum(!is.na(V1Temp))

## V2Temp: 변수명 종류, V2Number: 변수명의 갯수
V2Temp<-unique(KosisDataSet[,2])
V2Number<-sum(!is.na(V2Temp))

## 지역명과 변수명이 완전하게 매칭되는 데이터셋을 만든다
Bdata<-(matrix(NA, V1Number, V2Number, byrow=F))
Bdata<-as.data.frame(Bdata)
rownames(Bdata)<-t(unique( KosisDataSet[complete.cases(KosisDataSet[1]),][,1]))
colnames(Bdata)<-t(V2Temp)

## 원본자료를 지역명과 변수명이 매칭되는 데이터셋으로 옮기는 작업
for(i in 1:n){
for(j in 1:V1Number){
if(KosisDataSet$V1[i]==rownames(Bdata)[j])
for(k in 1:V2Number){
if(KosisDataSet$V2[i]==colnames(Bdata)[k]) {Bdata[j,][k]<-KosisDataSet$V3[i]}
}}}

return(Bdata)

}

