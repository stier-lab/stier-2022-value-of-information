#' Get number of years that the stock spent near A
#'
#' @param B.vec a vector of biomass values
#' @param threshold percentage distance within which to search for 
#' @param A critical threshold AKA "tipping point" where biomass growth rate goes negative
#'
#' @return a decimal value of the number of years that B was within threshold % of A
#' @export
#'
#' @examples
#' fakeB <- rnorm(100,mean = 50,sd=10)
#' dangerzone(B.vec = fakeB, A = 30, thresh = 0.2)
dangerzone <- function(B.vec, A, thresh){
  if(A == 0){print("A = 0, FYI")}
  if(A < 0){stop("A cannot be less than zero")}

  nyears <- length(B.vec)
  x <- c(A*(1-thresh), A*(1+thresh))
  in.zone <- length(which(B.vec >= x[1] & B.vec <= x[2]))
  prop.years <- in.zone / nyears
  return(prop.years)
}
