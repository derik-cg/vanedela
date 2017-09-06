#this produces a table for percentiles

#this is the function that produces a given percentile for all 
#of the reactives. This does not produce the confidence
#intervals

perc<-function(daba,lvl){
  #create a dataframe for a given set of percentiles
  dat<-matrix(NA,nrow=180,ncol=7)
  rownames(dat)<-names(vanedela[5:184])
  colnames(dat)<-c("10","25","50","75","90","min","max")
  quantile(vanedela)
  name<-paste("perc",lvl,sep="")
  (name,matrix(NA,nrow=180,ncol=7))
  
}