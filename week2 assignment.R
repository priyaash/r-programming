pollutantmean<- function(directory, pollutant, id= 1:332){
  filelist<- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  values<- numeric()
  for (i in id) {
    data<- read.csv(filelist[i])
    values<-c(values, data[[pollutant]])
    
  }
  mean(values, na.rm = TRUE)
}


complete<- function(directory, id = 1:332){
  filelist<- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  nobs<- numeric()
  for (j in id) {
    data<- read.csv(filelist[j])
    nobs<- c(nobs, sum(complete.cases(data)))
  }
  data.frame(id, nobs)
}








corr<- function(directory, threshold = 0){
  
  complete<- function(directory, id = 1:332){
    filelist<- list.files(path = directory, pattern = ".csv", full.names = TRUE)
    nobs<- numeric()
    for (j in id) {
      data<- read.csv(filelist[j])
      nobs<- c(nobs, sum(complete.cases(data)))
    }
    data.frame(id, nobs)
  }
  
  dat_req<-complete(directory)
  dat_req<-dat_req[dat_req$nobs>= threshold,]
  
  cor_vect<- numeric()
  
  filelist1<- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  if(nrow(dat_req)>0){
     for (i in dat_req$id) {
       mon_data<-read.csv(filelist1[i])
       mon_data<-mon_data[!is.na(mon_data$sulfate) & !is.na(mon_data$nitrate), ]
       cor_vect<- c(cor_vect, cor(mon_data$sulfate, mon_data$nitrate))
     }
      
    return(cor_vect)}
    
}

  

