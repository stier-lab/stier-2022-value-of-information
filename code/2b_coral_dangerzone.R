#' Get number of years that coral cover spent in danger zone
#'
#' @param C.vec a vector of coral cover values
#' @param threshold a fixed threshold for coral cover
#' @param A.coral critical threshold AKA "tipping point" where coral growth rate goes negative
#'
#' @return a fraction of years in the danger zone
#' @export
#'
#' @examples
#' fakeC <- rnorm(100, mean = 50, sd = 10)
#' coral_dangerzone(C.vec = fakeC, A = 20, thresh = 0.8 * 60)
coral_dangerzone <- function(C.vec, A, thresh){
  if(A == 0){print("A = 0, FYI")}
  if(A < 0){stop("A cannot be less than zero")}

  nyears <- length(C.vec)

  # Count years in danger zone (below threshold but not collapsed)
  in.zone <- length(which(C.vec <= thresh & C.vec > A))

  # Count years where coral hasn't crashed below Allee threshold
  nyears.not.crashed <- length(which(C.vec > A))

  # Proportion of non-crashed years spent in danger zone
  prop.years <- in.zone / nyears.not.crashed

  return(prop.years)
}
