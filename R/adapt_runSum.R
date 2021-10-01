#' Adaptive Running Sum
#'
#' This function calculates the running sum of a given vector for a given window size.
#' @param x the given vector
#' @param n window size of the running sum: Default to 1. When n=1, returns x. When length(x)< n, return cumulative sum
#' @keywords running sum
#' @export
#' @examples
#' adapt_runSum(c(1, 2, 3, 4, 5))

adapt_runSum <- function(x,n=1){
  # x: vector for which to calculate the adaptive running sum
  # n: window size of the running sum 
  # when n=1, the function returns the vector itself 
  # if length(x)< n, then the function returns cumulative sum
  csum <- cumsum(x)
  L <- length(x)
  if (n< L ) {
    csum[(n+1): L] <- csum[(n+1):L]-csum[1:(L-n)]
  }
  return(csum)
}