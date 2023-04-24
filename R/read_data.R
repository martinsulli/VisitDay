#' @title Function to read and format camera trap data
#' @description This function let's the user select the file to read in, then checks column names and formats times so they are numeric. If names in data don't match expected ones, there are interactive prompts to select the one with time and species data.
#' @author Martin Sullivan

read_data<-function(){
  dat<-read.csv(file.choose())
  if("Species"%in%names(dat) & "Time"%in% names(dat)){

  }else{
    for(i in 1:ncol(dat)){
      print(names(dat)[i])
      ans<-menu(c("Species","Time","Something else"),title="Does this column have")
      ifelse(ans==1,names(dat)[i]<-"Species",ifelse(ans==2,names(dat)[i]<-"Time",names(dat)[i]<-names(dat)[i]))
    }
  }
  defaultW <- getOption("warn")
  options(warn = -1)
  dat$Time<-sub(":","",dat$Time)
  dat$Time[grep("am",dat$Time)]<-paste0(substr(dat$Time[grep("am",dat$Time)],1,nchar(dat$Time[grep("am",dat$Time)])-2),"00")
  dat$Time[grep("AM",dat$Time)]<-paste0(substr(dat$Time[grep("AM",dat$Time)],1,nchar(dat$Time[grep("AM",dat$Time)])-2),"00")
  dat$Time[grep("pm",dat$Time)]<-(as.numeric(substr(dat$Time[grep("pm",dat$Time)],1,nchar(dat$Time[grep("pm",dat$Time)])-2))+12)*100
  dat$Time[grep("PM",dat$Time)]<-(as.numeric(substr(dat$Time[grep("PM",dat$Time)],1,nchar(dat$Time[grep("PM",dat$Time)])-2))+12)*100
  dat$Time<-as.numeric(dat$Time)/100
  missing.t<-dat$Time[is.na(dat$Time) | dat$Time < 0 | dat$Time > 24]
  if(length(missing.t)>0){
    dat$Time[dat$Time%in%missing.t]<-runif(length(missing.t),0,24)
    print("I couldn't read some times so have guessed a value for them. You may want to check the format of your data and read it in again.")
  }
  options(warn = defaultW)
  dat
}
