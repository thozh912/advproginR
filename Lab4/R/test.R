test <- function(x,W){
  reading <-x[,1,drop = FALSE]
  dates <- x[,2]
  
  for(i in 1:W){
    print(reading)
  }
  return(1)
}