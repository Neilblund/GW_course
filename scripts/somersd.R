#Somers D function
somersD<-function(y, x){
  tab<-table(x, y)
  concordant<-0
  discordant<-0
  untied<-0
  for(i in 1:(nrow(tab))){
    for(j in 1:(ncol(tab))){
      untied<-untied + tab[i,j] * (sum(tab) - sum(tab[1:i,]))
      if(i < nrow(tab) & j <ncol(tab)){
        concordant<-concordant + tab[i,j] * sum(tab[(i+1):nrow(tab),(j+1):ncol(tab)])
      }
      if(i< nrow(tab) & j>1){
        discordant<-discordant + tab[i,j] * sum(tab[(i+1):nrow(tab), 1:j-1])
      }
    }
  }
  Dyx<-(concordant-discordant)/untied
  return(Dyx)
  
}

#bootstrapping for CIs
bootDyx<-function(y, x, n=1000, level=.975, seed=100, full=FALSE){
  set.seed(seed)
  
  complete<-complete.cases(cbind(y,x))
  y<-y[complete]
  x<-x[complete]
    
  boots<-vector("numeric", length=n)
 
  for(i in 1:n){
    ind<-sample(length(y), replace=T)
    boots[i]<-somersD(y[ind], x[ind])
    
  }

  
  if(full==FALSE){
    sd<-sd(boots, na.rm=T)
    bound<- sd * qnorm(level)
    mean<-mean(boots,na.rm=T)
    boots<-c("mean"=mean, "sd"=sd, "p"=pnorm(abs(mean)/sd, lower.tail=F) ,"lower.bound"=mean-(bound),  "upper.bound"=mean+bound)
    
  }
  
  return(boots)
  
}
