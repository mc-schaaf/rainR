# computes the index of the peak velocity of a line,
# defined by arrays of x and y coordinates, and assumed to be equidistant in time

indexMaxDist <- function(x_vector, y_vector) {
  ds_real = sqrt(
    ( ( x_vector[2:length(x_vector)] - x_vector[1:(length(x_vector)-1)] )^2) +
      ( ( y_vector[2:length(y_vector)] - y_vector[1:(length(y_vector)-1)] )^2)
  )
  return(rep(which.max(ds_real), length(x_vector)))

}
