## NORMALIZATION FUNCTIONS
#scale 0 1

scale_0_1<-function(data, columns=30){
  for(i in 1:columns){
    data[,i]<-(data[,i]- min(data[,i]))/(max(data[,i])-min(data[,i]))
  }
  return(data)
}


# quant norm ->data is ranked, sortet, and every rank gets respective mean over all markers. Here, ties all get min value
quantile_norm<-function(data){
  rnk<-apply(data, 2, rank, ties.method= "min")
  data_sorted<-apply(data, 2, sort)
  sorted_means<-rowMeans(data_sorted)
  norm<-matrix(sorted_means[rnk], ncol=30)
  dimnames(norm)<-dimnames(data)
  return(as.data.frame(norm))
}

# generic function for quant norm: ties get means! only matrices
#preprocessCore::normalize.quantiles()


# quantile scaling. everything between lower and upper quantile bound gets scaled 0 - 1, all above or below gets 0/1
quantile_scaling<-function(data, lower=0.01, upper=0.99, columns=1:30){
  for(i in columns){
    data[,i]<-(data[,i]-quantile(data[,i, drop=TRUE], probs=c(lower)))/(quantile(data[,i, drop=TRUE], probs = c(upper))-quantile(data[,i, drop=TRUE], probs=c(lower)))
  }
  data[1:30][data[1:30]>1]<-1
  data[1:30][data[1:30]<0]<-0
  return(data)
}


# rank transform, adapted from TU: Ranked data, divided by total samples, converted into quantiles if standard normal distribution via qnorm
rank_norm<-function(data){  
  data <- apply(data,2,rank)/(nrow(data)+1)
  data <- apply(data,2,qnorm)
  data<-as.data.frame(data)
  return(data)
}


# IQR normalization uses interquartile range: median becomes 0, IQR becomes 1
IQR_norm<-function(data, columns=30){
  for(i in 1:columns){
    data[,i]<-(data[,i]-median(data[,i, drop=TRUE]))/(quantile(data[,i, drop=TRUE])[4]-quantile(data[,i, drop=TRUE])[2])
  }
  return(data)
}

#row z score
zscore<-function(data, columns=30, columnwise=TRUE){
  if(columnwise==TRUE){
    data_t<-as.data.frame(t(data[1:columns]))
    mean<-apply(data_t, 1, mean)
    sd<-apply(data_t, 1, sd)
    norm<-(data_t-mean)/sd
    return(cbind(as.data.frame(t(norm)), data[(columns+1):ncol(data)]))
  }
  else{
    mean<-apply(data[1:columns], 1, mean)
    sd<-apply(data[1:30], 1, sd)
    norm<-(data[1:30]-mean)/sd
    return(cbind(as.data.frame(norm), data[(columns+1):ncol(data)]))
  }
  
}
