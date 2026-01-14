GroupSmallClusters <- function(ids, SNN, verbose = TRUE, min_cell=10) {
  set.seed(12345)
  memory.limit(9999999999999)
  

  
  # identify cells
  cells <- c()
  cells <- names(x = which(x = table(ids) < min_cell))
  cells <- intersect(x = unique(x = ids), cells)

  # calculate connectivity of cells to other clusters, add cells
  # to cluster it is most connected to
  cluster_names <- as.character(x = unique(x = ids))
  cluster_names <- setdiff(x = cluster_names, y = cells)
  connectivity <- vector(mode = "numeric", length = length(x = cluster_names))
  names(x = connectivity) <- cluster_names
  new.ids <- ids


  
  for (i in cells) {
    gc()
    #print(paste("checking cluster", i))
    i.cells <- which(ids == i)
    
    pb <- progress_bar$new(
      format = paste("  checking cluster ", i,  " [:bar] :percent estimated time remaining: :eta"),
      total = length(i.cells)*length(cluster_names), clear = FALSE, width = 100)
    
    for(h in 1:length(i.cells)){
      
    for (j in cluster_names) {
      pb$tick()
     # print(paste("checking cell", h, "in cluster", i, "for connectivity with cluster", j))
      j.cells <- which(ids == j)
      subSNN <- SNN[i.cells[h], j.cells]

      if (is.object(x = subSNN)) {
        connectivity[j] <- sum(subSNN) / (nrow(x = subSNN) * ncol(x = subSNN))
      } else {
        connectivity[j] <- mean(x = subSNN)
      }
    }
      m <- max(connectivity, na.rm = T)
      mi <- which(x = connectivity == m, arr.ind = TRUE)
      closest_cluster <- sample(x = names(x = connectivity[mi]), 1)
      #print(connectivity)
     # print(paste("closest cluster to cell", h, "in cluster", i," is:", closest_cluster))
      ids[i.cells[h]] <- closest_cluster
      
    }

  }
  if (length(x = cells) > 0 && verbose) {
    message(paste(
      length(x = cells),
      "insufficient clusters identified.",
      length(x = unique(x = ids)),
      "final clusters."
    ))
  }
  return(ids)
}
