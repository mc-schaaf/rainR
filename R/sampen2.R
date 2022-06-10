#' @title SAMPle ENtropy
#'
#' @description computes the sample Entropy, as based on a Matlab script of Roland Pfister.
#' The matlab script can be found at https://osf.io/am6yp/
#' and the corresponding publications at https://doi.org/10.3758/s13428-020-01409-0
#' and https://doi.org/10.1016/j.cognition.2014.07.012
#'
#' @param timeseries_array input signal vector
#' @param dimensions maximum template length (default M=5).
#' Sometimes also called "embedding dimensions"
#' @param tolerance matching threshold (default r=.2)
#' @param standardize whether the tolerance is to be understood as absolute
#' values or as standardized values
#'
#' @return returns an array of length "dimensions" with the respective sampEns
#' (0,1,..,dimensions-1)
#'
#' @examples
#'
#' data("dat_one_trajectory")
#' sampen2(dat_one_trajectory$xvals)
#'
#' @export
#'

sampen2 <- function(timeseries_array,
                   dimensions = 5,
                   tolerance = 0.2,
                   standardize = FALSE) {
  # input conversion to mirror variable names of RP
  y <- timeseries_array
  M <- dimensions
  r <- tolerance

  # possibly: standardization
  if (standardize) {
    y <- y - mean(y)
    y <- y / sd(y)
  }

  # allocate counter variables
  n <- length(y)
  lastrun <- rep(0, n)
  run <- rep(0, n)
  A <- rep(0, M)
  B <- rep(0, M + 1)
  B[1] <- n * (n - 1) / 2

  # loop over all possible pairs of numbers
  for (i in 1:(n - 1)) {
    for (j in 1:(n - i)) {
      run[j] <- 0
      if (abs(y[j + i] - y[i]) < r) {
        run[j] <- lastrun[j] + 1
        M1 <- min(c(M, run[j]))

        for (m in 1:M1) {
          A[m] <- A[m] + 1
          if ((j + i) < n) {
            B[m + 1] <- B[m + 1] + 1
          }
        }
      }
    }
    lastrun[1:(n - i)] <- run[1:(n - i)]
  }

  B <- B[1:(length(B) - 1)]

  return(-log(A / B))

}
