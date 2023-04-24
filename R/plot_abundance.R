#' @title Function to plot species richness or abundance
#' @description This function plots total abundance or species richness in time bins.
#' @param data Object with data in. Must have columns called Species and Time.
#' @param type Either abundance or richness.
#' @param interval Time interval (in hours) to group observations in.
#' @param col Colour to plot bars in.
#' @author Martin Sullivan

plot_abundance<-function(data=dat, type="abundance",interval=3,col="black"){
  v<-seq(0,24,by=interval)
  tint<-findInterval(data$Time,v)
  if(type=="abundance"){
    ds<-tapply(data$Species,tint,length)
  }else{
    ds<-tapply(data$Species,tint,function(x)length(unique(x)))
  }
  res<-data.frame("Time"=v,"IntID"=seq(1:length(v)))
  res$Value<-ds[match(res$IntID,names(ds))]
  res$Value[is.na(res$Value)]<-0
  par(mar=c(4,4,4,4))
  barplot(res$Value,ylab=type,names.arg=res$Time,col=col)
}
