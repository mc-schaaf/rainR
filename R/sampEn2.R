#' @title sampen2
#'
#' @description computes the sample Entropy, as I understood it
#' comes with ABSOLUTELY NO WARRANTY, not certain whether it is completely correct
#'
#' @param timeseries_array array of numbers over which the sampEn is to be computed
#' @param dimensions number of embedding dimensions for which to compute the sampEn.
#' Sometimes also called "template length"
#' @param tolerance the tolerance for the comparisons of two number sequences
#' @param standardise whether to standardize the timeseries_array
#'
#' @return a single number, the sample Entropy for the given parameters
#'
#' @examples
#'
#' data("dat_one_trajectory")
#' sampen2(dat_one_trajectory$xvals)
#'
#' @export
#' @importFrom stats "sd"
#'

sampen2 <- function(timeseries_array,
                    dimensions = 2,
                    tolerance = 0.2 * sd(timeseries_array),
                    standardise = FALSE) {
  # input conversion to mirror variable names of RP
  y <- timeseries_array
  M <- dimensions
  r <- tolerance

  # possibly: standardisation
  if (standardise) {
    y <- y - mean(y)
    y <- y / (sqrt(mean(y ^ 2)))
  }

  N <- length(y) - M
  mat_m <- 0              # counter for matches of length M
  mat_m1 <- 0             # counter for matches of length M+1

  for (i in 1:(N - 1)) {
    for (j in (i + 1):N) {
      # for each possible pair of starting points of a vector of length M+1
      for (k in 0:(M)) {
        # test whether the vector of length M and the vector of length M+1 are within the tolerance
        if (abs(y[i + k] - y[j + k]) > r) {
          break
        }
        if ((k + 1) == M) {
          mat_m <- mat_m + 1
        }
        if ((k + 1) > M) {
          mat_m1 <- mat_m1 + 1
        }
      }
    }
  }

  return(-log(mat_m1 / mat_m))
}
