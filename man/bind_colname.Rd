\name{bind_colname}
\alias{bind_colname}
\title{Bind dataset and variable names}
\usage{
bind_colname()
}
\description{
Bind dataset and variable names
}
\examples{
   
   dataset<-'Adata'
   selecting_variables<-'V1, V15, V21'  
   
   <How to Use-1> 
   bind_colname(dataset, selecting_variables)  
   Print Output: 'Adata$V1,Adata$V15,Adata$V21' -> Copy and use it for cbind  
   
   <How to Use-2> 
   bind_colname(dataset, selecting_variables, Any number except 0 ) 
   Print Output: 'V1','V15','V21'  

}