#' Implements the Euclidean algorithm
#' 
#' \code{euclidean} returns the greatest common divisor of two integer numbers.
#' 
#' \code{euclidean} takes two integer numbers, \code{a} and \code{b}, and it 
#' divides the largest with the smallest one. Then the function takes the 
#' remainder and divides the smallest number between \code{a} and \code{b} with 
#' the remainder. This process continues until the function encounters a 
#' division where the remainder is zero and it returns the last denominator of 
#' the division.
#' 
#' @param a Should be a non negative integer.
#' @param b Should also be a non negative integer.
#'   
#' @return Returns the greatest common divisor between \code{a} and \code{b}.
#'   
#' @examples euclidean(5,18) returns 1
#' 
#' @references \url{http://en.wikipedia.org/wiki/Euclidean_algorithm}
#'   
#' @export
#' 
#' 
euclidean <- function(a,b){
  a <- as.integer(a)
  b <- as.integer(b)
  stopifnot( is.integer(a) , is.integer(b) , length(a) == 1 , length(b) == 1, a != 0, b != 0 )
  while(b != 0){
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}


