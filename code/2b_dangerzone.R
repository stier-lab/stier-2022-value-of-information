#' Get number of years that the stock spent near A
#'
#' @param B.vec a vector of biomass values
#' @param threshold a fixed number of biomass units from A
#' @param A critical threshold AKA "tipping point" where biomass growth rate goes negative
#'
#' @return a fraction of years below the threshold
#' @export
#'
#' @examples
#' fakeB <- rnorm(100,mean = 50,sd=10)
#' dangerzone(B.vec = fakeB, A = 30, thresh = 10)
dangerzone <- function(B.vec, A, thresh){
  if(A == 0){print("A = 0, FYI")}
  if(A < 0){stop("A cannot be less than zero")}
  nyears <- length(B.vec)
  in.zone <- length(which(B.vec <= 0.8*Bmsy & B.vec>1))
  nyears.not.crashed <- length(which(B.vec>1)) #AS changed this to >1 from >A bc getting prop.years>1
  prop.years <- in.zone / nyears.not.crashed
  return(prop.years)
}
