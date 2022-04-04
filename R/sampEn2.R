# computes the sample Entropy, as I understood it
# comes with ABSOLUTELY NO WARRANTY, not certain whether it is completely correct

sampEn2 <- function(timeseries_array,
                    dimensions = 2,
                    tolerance = 0.2 * sd(timeseries_array),
                    standardise=F, tidy=F) {

  # input conversion to mirror variable names of RP
  y = timeseries_array
  M = dimensions
  r = tolerance

  # possibly: standardisation
  if(standardise){
    y = y - mean(y)
    y = y / (sqrt(mean(y^2)))
  }

  N = length(y) - M
  mat_m = 0              # counter for matches of length M
  mat_m1 = 0             # counter for matches of length M+1

  for (i in 1:(N-1)) {
    for (j in (i+1):N) { # for each possible pair of starting points of a vector of length M+1
      for (k in 0:(M)) {
        # test whether the vector of length M and the vector of length M+1 are within the tolerance
        if (abs(y[i+k] - y[j+k]) > r) {
          break
        }
        if ((k+1) == M) {
          mat_m = mat_m + 1
        }
        if ((k+1) > M) {
          mat_m1 = mat_m1 + 1
        }
      }
    }
  }

  sampen = -log(mat_m1/mat_m)

  if(!tidy){
    return(sampen)
  } else {
    return(rep(sampen, length(timeseries_array)))
  }
}

