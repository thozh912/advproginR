djikstra <- function(graph, init_node){
  stopifnot(is.data.frame(graph))
  
  adjecent_to_in_unvisited <- function(v1,v2,node,unvisited){
    orderlist <- which(v1 == node)
    neighbours <- v2[orderlist]
    result <- neighbours[ neighbours %in% unvisited]
    return(result)
  }

  edge_value <- function(num1,num2,graph){
    stopifnot(is.data.frame(graph))
    for(i in 1:length(graph[[1]])){
      for(j in 1:length(graph[[2]])){
        if(graph[[1]][i] == num1 & graph[[2]][j] == num2 & i == j){
          place <- i
          return(graph[[3]][i])
        }
      }
    }
    return("edge not found")
  }
  dist <- rep(0,length(unique(graph[[1]])))
  resultvec <- c()
  nodevec <- c()
  unvisited_nodes <- c()
  
  for(i in unique(graph[[1]])){
    if( init_node != i){
      
      dist[i] <- Inf 
    }
    unvisited_nodes <- c(unvisited_nodes,i)
  }
  
  
  while(length(unvisited_nodes) != 0){
    
    min_dist_index <- order(dist)[1]
    
    min_dist_value <- dist[min_dist_index]
    
    min_dist_node <- unvisited_nodes[min_dist_index]
    
    dist <- dist[-min_dist_index]
    
    unvisited_nodes <- unvisited_nodes[-min_dist_index]
    
    
    for(nb in adjecent_to_in_unvisited(graph[[1]],graph[[2]],min_dist_node,unvisited_nodes)){
      
      alt <- min_dist_value + edge_value(min_dist_node,nb,graph)
      
      if(alt < dist[which(unvisited_nodes == nb)]){
        
        dist[which(unvisited_nodes == nb)] <- alt  
      }
    }
    resultvec <- c(resultvec,min_dist_value)
    nodevec <- c(nodevec,min_dist_node)
    resultvec2 <- resultvec[nodevec] 
  }
  return(resultvec2)
}
