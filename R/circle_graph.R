#KHD
circle_graph<-function(metadeconfoundR_long, size_nodepoint=4, size_text=3, linewidth=0.5, stroke_sig=1.25, backbone=NULL){
edges <- data.frame(from="origin", 
                   # to=unique(metadeconfoundR_long[!is.na(metadeconfoundR_long$Qs),]$metaVariable) %>% as.character())
                    to=unique(metadeconfoundR_long[metadeconfoundR_long$Qs < 0.1&!is.na(metadeconfoundR_long$Qs),]$metaVariable) %>% as.character())
vertices  <-  data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) , 
  value = 1
)


# Let's add a column with the group of each name. It will be useful later to color points
vertices$group  <-  edges$from[ match(vertices$name, edges$to) ]

edge_list <- lapply(unique(metadeconfoundR_long$feature), function(x){
  tmp <- metadeconfoundR_long %>% dplyr::filter(status != "NS" & feature == x)
  
  #calculate min and max Ds for legend
min(na.omit(tmp$Ds[!is.infinite(tmp$Ds)]))->min_d
max(na.omit(tmp$Ds[!is.infinite(tmp$Ds)]))->max_d

  tmp_edge <- lapply(tmp %>% dplyr::filter(grepl("C:", status)) %>% dplyr::pull(status), function(y){
    unlist(strsplit(y, split = ": "))[2]
  })
  names(tmp_edge) <- tmp %>% dplyr::filter(grepl("C:", status)) %>% dplyr::pull(metaVariable)
  
  if(length(tmp_edge)>0){
  
  edge_df <- data.frame()
  
  lapply(names(tmp_edge), function(x){
    tmp <- unlist(strsplit(tmp_edge[[x]], split = ", "))
    tmp_df <- data.frame(meta = rep(x, length(tmp)), confounder = tmp)
    if(nrow(edge_df) == 0) edge_df <<- tmp_df
    else edge_df <<- rbind(edge_df, tmp_df)
  })
  print(tmp %>% dplyr::filter(!grepl("C:", status)) %>% dplyr::pull(metaVariable))
  nodes <- tmp[,"metaVariable", drop = F]
  
  output <- list(edges = edge_df, vertices = nodes, others = tmp)


  # n.edges <- nrow(edges)
  # while(nrow(edges)-n.edges != nrow(output$edges)){
  # d2 <- data.frame(from=output$edges$meta[output$edges$meta %in% edges$to & !output$edges$meta %in% edges$from], 
  #                  to=output$edges$confounder[output$edges$meta %in% edges$to & !output$edges$meta %in% edges$from])
  # edges <- rbind(edges, d2)
  # }
  
  # create a dataframe with connection between leaves (individuals)
  connect <- data.frame( from=output$edges$confounder , to=output$edges$meta)
  connect$value <- 1
  
  vertices  <-  data.frame(
    name = unique(c(as.character(edges$from), as.character(edges$to))) , 
    value = 1
  ) 
  # Let's add a column with the group of each name. It will be useful later to color points
  vertices$group  <-  edges$from[ match(vertices$name, edges$to) ]
  vertices$eS <- output$others[match(vertices$name, output$others$metaVariable),]$Ds
  vertices$size <- abs(output$others[match(vertices$name, output$others$metaVariable),]$Ds)
  vertices$edge <- sapply(output$others[match(vertices$name, output$others$metaVariable),]$status, function(x){
    if (is.na(x)) 0.5
    else if (x != "NS" & !grepl("C:", x)) stroke_sig
    else 0.5
  })
  
  vertices$id <- NA
  myleaves <- which(is.na( match(vertices$name, edges$from) ))
  nleaves <- length(myleaves)
  vertices$id[ myleaves ] <- seq(1:nleaves)[c(3:(nleaves),1,2)]
  vertices$angle <- 45 - 360 * vertices$id / nleaves
  
  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90

    vertices$hjust <- ifelse( vertices$angle < -45, 0, 1)
  
  # flip angle BY to make them readable
  vertices$angle <- ifelse(vertices$angle < -45, vertices$angle+180, vertices$angle)
  
  
  
  # The connection object must refer to the ids of the leaves:
  mygraph <- igraph::graph_from_data_frame( edges, vertices=vertices )
  
  # The connection object must refer to the ids of the leaves:
  from  <-  match( connect$from, vertices$name)
  to  <-  match( connect$to, vertices$name)
  
  # color palette
  color_graph_fun <- circlize::colorRamp2(c(0,1), c("black", "#F5F5F5"))
  color_graph <- color_graph_fun(seq(0,1,length.out = 100))
  names(color_graph) <- seq(0,1,length.out = 100)
  
  color_node_fun <- circlize::colorRamp2(c(-1,0,1), c("blue", "white", "red"))
  color_node <- color_node_fun(seq(-1,1,length.out = 100))
  names(color_node) <- seq(-1,1,length.out = 100)
  
  # Basic usual argument
  plot <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE)
  
  layout_df <- plot$data
  # Rotate x, y coordinates by rotation_rad around origin (0,0)
  rotate_coords <- function(x, y, angle) {
  x_new <- x * cos(angle) - y * sin(angle)
  y_new <- x * sin(angle) + y * cos(angle)
  data.frame(x = x_new, y = y_new)
  }
  
  rotated_coords <- rotate_coords(layout_df$x, layout_df$y, (30*pi/180))
  
  
# Update coordinates in layout data
layout_df$x <- rotated_coords$x
layout_df$y <- rotated_coords$y

# Update label angles and justification:
# Calculate new angle in degrees from coordinates:
# atan2 gives angle in radians; convert to degrees
new_angles <- atan2(layout_df$y, layout_df$x) * 180 / pi

# For label orientation and hjust, similar logic you have but with new_angles
layout_df$angle <- ifelse(new_angles < -90, new_angles + 180, new_angles)
layout_df$hjust <- ifelse(new_angles < -90, 0, 1)

# Replace plot$data with rotated and adjusted layout_df
plot$data <- layout_df
  
  if(!is.null(backbone)){
    plot<-plot+geom_path(aes(x=c(x[length(x)], x[-1])*1.05, y=c(y[length(y)], y[-1])*1.05), linewidth=14, color=backbone[x])
  
}

  plot<-plot + geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.9,aes(colour = ..index..) , width=linewidth ,tension = 0.5
                     #arrow = arrow(angle = 30, length = unit(0.1, "inches"),ends = "last", type = "closed")
    ) +
    #scale_edge_colour_gradientn(colors = color_graph, values = names(color_graph)) +
    scale_edge_color_distiller(palette = "Greys", name="Confounder direction", breaks=c(0,1), labels=c("confounding", "confounded")) +
    geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, fill = eS, stroke = edge), 
                    shape = 21, color = "black", size = size_nodepoint, show.legend = TRUE) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                        labels = c("negative", "positive"),
                        limits=c(min_d, max_d),
                        breaks = c(min_d, max_d),
                         na.value = "white", 
                         name="Effect size") +
   # geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name, angle = angle, hjust=hjust), size=3, alpha=1) +
    geom_node_text(aes(x = x*1.2, y=y*1.2, filter = leaf, label=name, angle = node_angle(x,y), hjust="outward"), size=size_text, alpha=1) +
    theme_void() +
    #annotate(geom="text", label=x, x=-0.7, y=1.45, size=30, fontface="bold")+
    #annotate(geom="text", label=x, x=0, y=0, size=40, fontface="bold", color="#278046")+
    annotate(geom="text", label=x, x=0.1, y=-1.77, size=30, fontface="bold")+
    theme(
    #  plot.margin=unit(c(1,1,1,1),"cm"),
      panel.background = element_rect(fill = "transparent", colour = NA),  
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.position = c(0.305, 0.09),
      legend.title = element_text(size=50, hjust=0.5),
      legend.text = element_text(size=50),
      legend.direction = "horizontal",
      legend.key.width = unit(3.5, "cm"),
      legend.key.size = unit(1, "cm"),
      legend.title.position = "top",
      legend.box = "vertical",
      legend.frame = element_rect(color="black", linewidth = 2),
      aspect.ratio = 1
    ) +
    expand_limits(x = c(-2, 2), y = c(-2, 2))
  #+Seurat::NoLegend()


} else {
    return(tmp)
  }
})




return(edge_list)
}
