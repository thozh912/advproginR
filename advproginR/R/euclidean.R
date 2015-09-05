euclidean <- function(a,b){
  stopifnot( is.integer(a) , is.integer(b) , length(a) == 1 , length(b) == 1)
  while(b != 0){
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}


