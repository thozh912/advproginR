#' Implements the dijkstra algorithm
#' 
#' \code{dijkstra} returns the shortest distance from the inital node in a graph
#' to all other nodes.
#' 
#' Dijkstras algorithm is implemented in function \code{dijkstra} which takes 
#' first argument \code{graph} a three column data frame which represents a
#' weighted graph and second argument \code{init_node} an integer to represent
#' the initial node. It then proceeds to produce the shortest distances from an
#' initial node to the rest of the nodes in a given graph. It returns a vector
#' with the shortest distances in the order of the node numbering.
#' 
#' @param graph A data.frame object with three columns, representing a graph 
#'   with weighted edges. the column content are as follows:
#'   
#'   
#'   \describe{
#'   
#'   \item{1}{The numeric or integer numbers of the starting nodes of the edges in the
#'   graph}
#'   
#'   \item{2}{The numeric or integer numbers of the ending nodes of the edges in the graph}
#'   
#'   \item{3}{The numeric edge weights of the edges in the graph}
#'   
#'   }
#'   
#'   Each edge is considered to have two starting nodes and thus is listed two 
#'   times. This is required.
#'   
#' @param init_node The integer of the starting node from which dijkstras 
#'   algorithm proceeds to find the shortest distances to the other nodes.
#'   
#' @return \code{resultvec2} A numeric vector with the shortest distances to the
#'   other nodes, ordered according to the numbering of the nodes.
#'   
#' @examples
#' crosssquare <- data.frame(c(1,1,1,2,2,2,3,3,3,4,4,4),
#' c(2,3,4,3,4,1,4,1,2,1,2,3),c(1,6,2,5,3,1,2,6,5,2,3,2))
#' start <- 3
#' dijkstra(crosssquare, start)
#' 
#' @references \url{http://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#'   
#' @export


dijkstra <- function(graph, init_node){
  stopifnot(is.data.frame(graph) , length(names(graph)) == 3 , init_node %in% unique(graph[[1]]))
  
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

  }
  return(resultvec[order(nodevec)])
}
