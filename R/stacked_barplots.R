


library(dplyr)
library(ggplot2)

stacked_barplots<-function(data, 
                           title="", 
                           filename="stacked_barplot.tiff", 
                           order=FALSE, cluster, 
                           plot_type="barplot", 
                           cluster_order=FALSE, 
                           color=color_clusters, 
                           relative_to="MCT", 
                           ncells=NULL, 
                           width=1000, 
                           height=1000, 
                           ylab="frequency",
                           subscript=FALSE){
  
  data$timepoint[data$timepoint=="AB"]<-"Adult"
  data$timepoint[data$timepoint=="CB"]<-"Term"
  
  data$cluster<-cluster
  data_grouped<-data %>% group_by(cluster,timepoint) %>% summarise(freq = n())
  data_grouped<-as.data.frame(data_grouped)
  data_grouped<-na.omit(data_grouped)
  
  if(relative_to=="MCT"){
  for(i in unique(data_grouped$timepoint)){
  data_grouped[data_grouped$timepoint==i,]$freq<-(data_grouped[data_grouped$timepoint==i,]$freq)/sum(data$timepoint==i, na.rm=TRUE)
  } 
  } else {
     for(i in unique(data_grouped$timepoint)){ 
    data_grouped[data_grouped$timepoint==i,]$freq<-(data_grouped[data_grouped$timepoint==i,]$freq)/ncells[i]
     }
    }
    
  
  if(isFALSE(order)){
    order<-levels(factor(data_grouped$timepoint))
  }
  
  if(isFALSE(cluster_order)){
    cluster_order<-unique(data_grouped$cluster)
  }
  
 
  data_grouped$timepoint[data_grouped$timepoint=="Term"]<-"T"
  data_grouped$timepoint[data_grouped$timepoint=="Preterm"]<-"P"
  data_grouped$timepoint[data_grouped$timepoint=="Adult"]<-"A"
  
  data_grouped$freq<-data_grouped$freq*100
  

  
  stacked_barplot<-ggplot(data_grouped, aes(fill=factor(cluster, levels = cluster_order), y=freq, x=factor(timepoint, levels=order))) +
    geom_bar(position='stack', stat='identity')+
    scale_fill_manual("Celltype", values = color)+
    labs(title = title)+
    ylab(ylab)+
    theme_classic()+
      theme(legend.text = element_text(size=60), 
          legend.title = element_blank(), 
          axis.title = element_text(size=60),
          axis.text = element_text(size=60),
          plot.title = element_text(size=70),
          axis.title.x = element_blank(),
#          axis.text.x=element_text(angle=90, vjust=0.4),
          aspect.ratio = 1.5)
  
  if(subscript==TRUE){
    
    labels <- c(
  expression(TP1[C]),
  expression(TP1[P]),
  "TP2",
  "TP3"
    )
    
    stacked_barplot<-stacked_barplot + scale_x_discrete(labels=labels)
  }
  

  
  tiff(filename=filename, compression = "lzw", width=width, height=height)
  plot(stacked_barplot)
  dev.off()
}



