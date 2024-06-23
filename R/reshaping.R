# How To Use: df2<-reshaping(df, commoncol, repeating number) 

reshaping<-function(df, commoncol, repeating){

   if (base::missing(df)) {
	    cat("   V1<-c(1,2,3); V2<-c(10,11,12); V3<-c(20,21,22)   ", '\n')
		cat("   V4<-c(30,31,32); V5<-c(40,41,42); V6<-c(50,51,52)   ", '\n')
		cat("   V7<-c(60,61,62); V8<-c(70,71,72)   ", '\n')
		cat("   df<-cbind(V1,V2, V3,V4,V5,V6,V7,V8)   ", '\n')
		cat("   df={V1 V2 V3 V4 V5 V6 V7 V8}   ", '\n')
		cat("   *NOTE: V1과 V2는 공통, V3~V8을 2개씩 끊어서 데이터셋을 만든 후 rbind ", '\n')
	    cat("   *NOTE: V1 V2 V3 V4 : V1 V2 V5 V6 : V1 V2 V7 V8 - 3개의 데이터셋이 repeating", '\n')
		cat("   *NOTE: df2<-reshaping(df, 공통컬럼은 1~k까지, (k+1)~n컬럼을 끊어서 m개 PART로 나눌 경우) ", '\n')
		cat("   *NOTE: df2<-reshaping(df, k, m) ", '\n')
		return(cat("      ") ) }



vnames<-c('v1','v2','v3','v4','v5','v6','v7',
          'v8','v9','v10','v11','v12','v13','v14','v15','v16','v17',
          'v18','v19','v20','v21','v22','v23','v24','v25','v26','v27',
          'v28','v29','v30','v31','v32','v33','v34','v35', 'v36', 'v37',
		  'v38','v39','v40','v41','v42','v43','v44','v45', 'v46', 'v47',
		  'v48', 'v49', 'v50', 'v51', 'v52', 'v53', 'v54', 'v55', 'v56',
		  'v57', 'v58','v59', 'v60', 'v61', 'v62', 'v63', 'v64', 'v65', 'v66',
		  'v67', 'v68','v69','v70','v71','v72','v73','v74','v75','v76','v77',
		  'v78','v79','v80','v81','v82','v83','v84','v85','v86','v87','v88','v89')


totalcol<-ncol(df)
blockcol<-totalcol-commoncol
repeatcol<-blockcol/repeating
tmp_colnames<-commoncol+repeatcol

for(i in 1:repeating){
  startcol<-repeatcol*(i-1)+commoncol+1
  endcol<-repeatcol*i+commoncol
  tmp<-df[, c(1:commoncol,  startcol:endcol) ]


    for(x in 1:tmp_colnames){
      colnames(tmp)[x]<-vnames[x]
    }

    
  if(i==1) {
    Totaltmp<-tmp
    next}
  Totaltmp<-rbind(Totaltmp, tmp)
}

return(Totaltmp)
}

