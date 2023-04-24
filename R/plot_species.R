#' @title Function to plot species observations against time
#' @description This function plots when each species was observed.
#' @param data Data object. Must have columns "Species" and "Time".
#' @param col Colour to plot points in.
#' @author Martin Sullivan

plot_species<-function(data=dat,col="black"){
  data$Species<-tolower(data$Species)
  sp<-unique(data$Species)
  par(mar=c(4,8,1,1))
  par(mgp=c(2,0.5,0))
  par(las=2)
  plot(1,type="n",xlim=c(0,24),ylim=c(0,length(sp)),yaxt="n",xlab="Time",ylab="")
  axis(2,at=seq(1:length(sp)),labels=sp)
  for(i in 1:length(sp)){
    tmp<-data[data$Species==sp[i],]
    points(tmp$Time,rep(i,nrow(tmp)),pch=16,col=col)
  }
}
