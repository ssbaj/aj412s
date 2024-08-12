# Binding and printing Dataset name and Variable colnames

bind_colname<-function(dataset, tmp, indicator=0) {
  
if (base::missing(dataset)) {
    cat("   \033[1;36mBinding Names of Dataset and Variables \033[0m", '\n' )
	cat("   \033[1;36m<How to Use-1>\033[0m", '\n' )
    cat("   dataset<-\033[1;31m'Adata'\033[0m; selecting_variables<-\033[1;31m'V1, V15, V21'\033[0m ", '\n' )
    cat("   bind_colname(dataset, selecting_variables) ", '\n' )
    cat("   Print Output: \033[1;31m'Adata$V1,Adata$V15,Adata$V21'\033[0m -> Copy and use it for \033[1;31mcbind\033[0m ", '\n' )
    cat("   \033[1;36m<How to Use-2>\033[0m", '\n' )
    cat("   bind_colname(dataset, selecting_variables, Any number except 0 )", '\n' )
    return(cat("   Print Output: \033[1;31m'V1','V15','V21'\033[0m ", '\n' ))}
  
indexdataset<-dataset

bindX<-function(indexdataset, tmp, indicator) {
    tmp <- gsub("\\s", "", tmp)
    rcount<-c()
    countchar<-nchar(tmp)
    countchar
    counter_i <- 0
    tmp
    
    nword<-function(tmp){
      countchar<-nchar(tmp)
      for(i in 1:countchar){
        if(substr(tmp, i, i)  != ",") { counter_i <- counter_i+1}
        if(substr(tmp, i, i)  == ",") { rcount <- c(rcount, counter_i); counter_i<-0 }
        if(i==countchar) { rcount <- c(rcount, counter_i)}
      }
      return(rcount)
    }
    
    
    countword<-nword(tmp)
    Ncount<-length(countword)
    Ncount2<-Ncount
    
    start<-1
    end<-0
    r3=c()
    
    
    if(indicator ==0) {
      for(imcounting in 1:Ncount){
        if(imcounting==1) {
          start=1
          end=start + countword[imcounting]-1
        }
        
        if(imcounting>1) {
          start <- end + 2
		  end <- start + countword[imcounting]-1
        }
        
        sub_text<-substr(tmp, start, end)
        
        
        if(imcounting==Ncount2) {
          sub_text<-paste0(indexdataset,"$",sub_text)
        }
        else {
          sub_text<-paste0(indexdataset,"$",sub_text,",  ")
        }
        
        
        r3=c(r3, sub_text)
      }
      return(r3)
    }
    
    
    if(indicator !=0) {
      for(imcounting in 1:Ncount){
        if(imcounting==1) {
          start=1
          end=start + countword[imcounting]-1
        }
        
        if(imcounting>1) {
          start <- end + 2; end <- start + countword[imcounting]-1
        }
        
        sub_text<-substr(tmp, start, end)
        
        if(imcounting==Ncount2) {
          sub_text<-paste0("'",sub_text,"'")
        }
        else {
          sub_text<-paste0("'",sub_text,"', ")
        }
        
        
        r3=c(r3, sub_text)
      }
      return(r3)
    }
    
  }
  
  output <- capture.output( bindX(indexdataset, tmp, indicator) )
  output<-gsub('"', '', output)
  output<-gsub(' ', '', output)
  output <- gsub("[\r\n]", "", output)
  output <- gsub("\\[|\\]", "", output)
  output <- as.character(output)
  output <- as.vector(output)
  
  return(output)
  }
