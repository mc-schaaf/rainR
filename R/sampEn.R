#' @title sampen
#'
#' @description computes the sample Entropy, as based on a Matlab script of Roland Pfister.
#' comes with ABSOLUTELY NO WARRANTY, as this is solely a translation!
#'
#' @param timeseries_array array of numbers over which the sampen is to be computed
#' @param dimensions number of embedding dimensions for which to compute the sampen Sometimes also called "template length"
#' @param tolerance the tolerance for the comparisons of two number sequences
#' @param standardise whether the tolerance is to be understood as absolute values or as standardized values
#' @param tidy whether the output of the function should be RP-style or a single number so it can be applied with tidyverse's "mutate"
#'
#' @return returns an array of length "dimensions" with the respective sampEns or a single number, the last element of this array
#'
#' @examples
#'
#' data("dat_one_trajectory")
#' sampen(dat_one_trajectory$xvals)
#'
#' @export
#'

sampen <- function(timeseries_array,
                   dimensions = 5,
                   tolerance = 0.2,
                   standardise = T,
                   tidy = F) {
  # input conversion to mirror variable names of RP
  y = timeseries_array
  M = dimensions
  r = tolerance

  # possibly: standardisation
  if (standardise) {
    y = y - mean(y)
    y = y / (sqrt(mean(y ^ 2)))
  }

  #  allocate counter variables
  n = length(y)
  lastrun = rep(0, n)
  run = rep(0, n)
  A = rep(0, M)
  B = rep(0, M + 1)
  B[1] = n * (n - 1) / 2

  # loop over all possible pairs of numbers
  for (i in 1:(n - 1)) {
    for (j in 1:(n - i)) {
      run[j] = 0
      if (abs(y[j + i] - y[i]) < r) {
        run[j] = lastrun[j] + 1
        M1 = min(c(M, run[j]))

        for (m in 1:M1) {
          A[m] = A[m] + 1
          if ((j + i) < n) {
            B[m + 1] = B[m + 1] + 1
          }
        }
      }
    }
    lastrun[1:(n - i)] = run[1:(n - i)]
  }

  B = B[1:(length(B) - 1)]
  if (!tidy) {
    return(-log(A / B))
  } else {
    return(-log(A[dimensions] / B[dimensions]))
  }

}
