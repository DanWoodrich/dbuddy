#functions to create standard bins
library(foreach)

breakbins<-function(data,binlen,type,colname_dur = "Duration",rowtype= "db"){

  #condition: if the row has a higher duration than the max of the bins, break it into multiple until condition is satisfied
  
  data$index<-1:nrow(data)
  
  newrows<-foreach(n=1:nrow(data)) %do% {
  #for(n in 1:nrow(data)){
    row = data[which(data$index==n),]
    #go row by row. if duration > binlen, expand row, delete old row, and stich back in. 
    if(row[,colname_dur]>binlen){
      
      if(row[,colname_dur]%%binlen==0){
        binints = rep(binlen,row[,colname_dur]/binlen)
        binstarts = c(0,cumsum(binints))
      }else{
        secs = sum(rep(binlen,row[,colname_dur]/binlen))
        binints = c(rep(binlen,row[,colname_dur]/binlen),row[,colname_dur]-secs)
        binstarts= c(0,cumsum(binints))
      }
      startends<-matrix(nrow=length(binints),ncol=2)
      for(p in 1:length(binints)){
        startends[p,]<-c(binstarts[p],binints[p])
      }
      
      #nice, so now have intervals. create the rows
      
      if(rowtype=="FG"){
      rows = data.frame(row[,1:5],startends,row[,8:9])
      }else if(rowtype=="db"){
      rows = data.frame(row[,1],startends,row[,3:4],type)
      }
      data=data[-which(data$index==n),]
      return(rows)
    }else{
      return(NULL)
    }
    
    
  }
  newrows =do.call("rbind",newrows)
  
  if(!is.null(newrows)){
    if(rowtype=="FG"){
    colnames(newrows)[c(6,7)]<-c("SegStart","SegDur")
    }else if(rowtype=="db"){
    colnames(newrows)[c(2,3)]<-c("SegStart","SegDur")
    }
    data=rbind(newrows,data)
  }
    
  data<-data[order(data$index,data$SegStart),] #original order
  
  data$index<-NULL
  
  return(data)
  
}

make_standard_bins<-function(data){

  bincategories<-c("LOW","MID","HIGH")
  binlengths<-c(300,225,90)

  out=foreach(i=1:length(binlengths)) %do% {
    return(breakbins(data,binlengths[i],bincategories[i]))
  }
  
  out=do.call("rbind",out)
  return(out)
  
}

