sig2<-function(re, signal=999){
  if(signal !=999 ){
  tmp<-data.frame(VarCorr(re))$vcov
  print( data.frame(VarCorr(re)) )
  cat(" ---------------------- ", "\n")
  return(tmp)
  }

  if(signal==999) {
  tmp<-data.frame(VarCorr(re))$vcov
  return(tmp)}
}


