check_resolutions<-function(snn, resolutions, n_iterations=-1, objective_function="CPM"){

  pb <- progress_bar$new(
    format = "  leiden clustering resolution test [:bar] :percent estimated time remaining: :eta",
    total = length(resolutions), clear = FALSE, width = 100)
  
  leiden_list<-list()
  for (i in  resolutions) {
    pb$tick()
    set.seed(12345)
    leiden_list[[length(leiden_list)+1]]<-igraph::cluster_leiden(snn, resolution_parameter=i, n_iterations=n_iterations, objective_function=objective_function)
  }
  
  names(leiden_list)<-resolutions
  return(leiden_list)
}

plot_resolution_test<-function(leiden_list, umap, data){
  
library(igraph)
library(ggplot2)
  
pb <- progress_bar$new(
  format = "  creating pptx files [:bar] :percent estimated time remaining: :eta",
  total = length(leiden_list), clear = FALSE, width = 100)

officer::read_pptx()->doc
tryCatch({
for(i in 1:length(leiden_list)){
  
  pb$tick()
  

  members<-factor(membership(leiden_list[[i]]))
  plot<-ggplot2::ggplot(as.data.frame(umap), aes(x = V1, y = V2, color=members))+ 
    geom_point(size=0.1) + 
    theme_bw() + 
    scale_color_manual(values = color_clusters)+
    guides(color = guide_legend(override.aes = list(size = 3))) +
    theme(legend.text=element_text(size=8))+labs(title=paste("resolution = ", names(leiden_list)[i], sep=""))
  
  doc <- add_slide(doc, 'Title and Content', 'Office Theme') 
  doc <- ph_with(doc, value = plot, location=ph_location_fullsize())
  
  if(length(unique(members))>1){
    
    plot<-heatmap_cluster(data[1:30], members)
    doc <- add_slide(doc, 'Title and Content', 'Office Theme') 
    doc <- ph_with(doc, value = plot, location=ph_location_fullsize())
  }
  }
  
}, error=function(e){
  print(e)
  dev.off()
  })

return(doc)
}


merge_cells<-function(all_files, annotated_data, step){
  
  pb <- progress_bar$new(
    format = "  merging remaining cells  [:bar] :percent estimated time remaining: :eta",
    total = length(all_files), clear = FALSE, width = 100)
  
  for(i in 1:length(all_files)){
  #filter for chip data
  annotated_chip_data<-annotated_data[annotated_data$chip==names(all_files)[i],]

  #filter remianing data
  if(step=="main cell type"){
    set.seed(12345)
    suppressMessages(anti_join(all_files[[i]][["subsampled_data"]], annotated_chip_data)->remaining_data)
  } else{
  set.seed(12345)
  suppressMessages(anti_join(all_files[[i]][["subsampled_data_main_cell_types_annotated"]], annotated_chip_data)->remaining_data)
  }
  remaining_data$timepoint<-rep(annotated_chip_data$timepoint[1])
  remaining_data$group<-rep(annotated_chip_data$group[1])
  
  remaining_data$subcluster<-rep("error", nrow(remaining_data))
  
  #knn clustering
  if("cluster" %in% colnames(remaining_data)){
  for(i in unique(remaining_data$cluster)){
    set.seed(12345)
    knn<-class::knn(train=annotated_chip_data[annotated_chip_data$cluster==i,][1:30], test=remaining_data[remaining_data$cluster==i,][1:30], cl=annotated_chip_data[annotated_chip_data$cluster==i,]$subcluster, k=10, l = 0, prob = FALSE, use.all = TRUE)
    remaining_data[remaining_data$cluster==i,]$subcluster<-as.character(factor(knn))
    }
  }
  else{
    set.seed(12345)
    knn<-class::knn(train=annotated_chip_data[annotated_chip_data$cluster==i,][1:30], test=remaining_data[remaining_data$cluster==i,][1:30], cl=annotated_chip_data[annotated_chip_data$cluster==i,]$subcluster, k=10, l = 0, prob = FALSE, use.all = TRUE)
    remaining_data[remaining_data$cluster==i,]$subcluster<-as.character(factor(knn))
  }
  
  if(step!="main cell type"){
  if(isTRUE("stem cells"%in%all_files[[i]][["subsampled_data_main_cell_types_annotated"]][["cluster"]])){
    remaining_data[remaining_data$cluster=="stem cells",]$subcluster<-"stem cells"
  }
  }
  
  remaining_data$subcluster<-as.character(remaining_data$subcluster)
  annotated_chip_data$subcluster<-as.character(annotated_chip_data$subcluster)
  
  if(step=="main cell type"){
    all_files[[i]][["subsampled_data_main_cell_types_annotated"]]<-rbind(annotated_chip_data, remaining_data)
  } else{
  all_files[[i]][["subsampled_data_subcluster_annotated"]]<-rbind(annotated_chip_data, remaining_data)
  }
  
  rm(annotated_chip_data, remaining_data, knn)
  
  if(step=="main cell type"){
    all_files[[i]][["main_cell_type_factor"]]<-factor(all_files[[i]][["subsampled_data_main_cell_types_annotated"]]$cluster)
  } else{
  all_files[[i]][["subcluster_factor"]]<-factor(paste(all_files[[i]][["subsampled_data_subcluster_annotated"]]$cluster, "_", all_files[[i]][["subsampled_data_subcluster_annotated"]]$subcluster, sep=""))
  }
  }
  
  return(all_files)
}


frequencies<-function(data, filename="frequencies.xlsx", clustering, type=NULL){
  freq<-as.data.frame(unique(data[[clustering]]))
  colnames(freq)<-"cluster"
  
  for(i in 1:length(unique(data$chip))){
    freq[unique(data$chip)[i]]<-rep(0,length(unique(data[[clustering]])))
    temp<-data[data$chip==unique(data$chip)[i],]
    table(factor(temp[[clustering]]))->table
    if(is.null(type)){
    table<-table/nrow(temp)}
    freq[,ncol(freq)][match(names(table), freq$`cluster`)]<-table
  }
  
  freq<-t(freq)
  colnames(freq)<-freq[1,]
  freq<-freq[-1,]
  freq<-as.data.frame(freq)
  
  for(i in 1:ncol(freq)){
    freq[[i]]<-as.numeric(freq[[i]])
  }
  
  freq$chip<-rownames(freq)
  freq$timepoint<-data$timepoint[match(freq$chip, data$chip)]
  
  if(is.null(type)){
  freq[1:length(unique(data[[clustering]]))]<-freq[1:length(unique(data[[clustering]]))]*100}
  
  writexl::write_xlsx(freq, filename)
  gc()
}



marker_expression<-function(data, marker=NULL, celltype, prism_data=TRUE, single_plots=TRUE){
  # select the columns containing expression values and metadata
  
  if( is.null(marker)){
    expression_columns <- names(data)[1:30]
  }
  else{
    expression_columns<-marker
  }
  
  # convert chip and subcluster to factors
  data$chip <- factor(data$chip)
  data$subcluster <- factor(data$subcluster, level=unique(data$subcluster))
  
  significances<-as.data.frame(unique(data$subcluster))
  colnames(significances)<-"cluster"
  plots<-officer::read_pptx()
  
  # iterate over the marker genes
  for (gene in expression_columns) {
    
    officer::read_pptx()->doc
    
    # create a data frame with the mean expression values for the current marker gene
    mean_expression <- data %>% group_by(chip, subcluster, timepoint, group) %>% summarize(mean_expression = mean(!!as.name(gene)))
    mean_expression$timepoint<-paste(mean_expression$group, "_" ,mean_expression$timepoint, sep="")
    mean_expression$group<-NULL
    
    
    # split the data by subcluster
    split_data<-split(mean_expression, mean_expression$subcluster)
    
    #perform anova to get preliminary idea of data
    anova<-list()
    for(i in names(split_data)){
    summary(stats::aov(data=split_data[[i]], mean_expression~timepoint))[[1]][["Pr(>F)"]][1]->anova[[i]]
      
    }
    
    #reduce data per subcluister to just mean expression, cbind and name
    for(i in names(split_data)){
      split_data[[i]]<-split(split_data[[i]], split_data[[i]][["timepoint"]])
      for(m in order){
        split_data[[i]][[m]]<-split_data[[i]][[m]][4]
      }
      names<-names(split_data[[i]][order][!is.na(names(split_data[[i]][order]))])
      split_data[[i]]<-as.data.frame(cbind.fill(split_data[[i]][names]))
      colnames(split_data[[i]])<-names
    }
    
    if(single_plots==TRUE){
    for(i in names(split_data)){
     suppressWarnings(as.data.frame(data.table::melt(split_data[[i]]))->temp)
      plot<-ggplot(drop_na(temp), aes(x=variable, y=value))+geom_boxplot()+labs(title=paste(gene, "expression per timepoint in cluster", i, "P-value:", anova[i]))

      
      doc <- add_slide(doc, 'Title and Content', 'Office Theme') 
      doc <- ph_with(doc, value = plot, location=ph_location_fullsize())
      
    }
    }
    
    ggplot(mean_expression, aes(x=timepoint, y=mean_expression))+
      geom_boxplot()+facet_wrap(vars(subcluster))+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      labs(title=gene)->plot
    plots <- add_slide(plots, 'Title and Content', 'Office Theme') 
    plots <- ph_with(plots, value = plot, location=ph_location_fullsize())
    
    if(single_plots==TRUE){
    print(doc, target=paste(gene, "_", celltype, ".pptx", sep=""))}
    if(prism_data==TRUE){
    writexl::write_xlsx(split_data, paste(gene, "_", celltype, ".xlsx", sep=""))
    }
    significances[[gene]]<-unlist(anova)
    
  }
  
  significances<-as.data.frame(significances)
  rownames(significances)<-names(anova)
  
  if(single_plots==FALSE){
  print(plots, target=paste("faceted_plots_", celltype, ".pptx", sep=""))}
  writexl::write_xlsx(significances, paste("anova_p_values_", celltype, ".xlsx", sep=""))
}

