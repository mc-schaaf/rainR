#' @title indexMaxAcceleration
#'
#' @description computes the index of the peak acceleration of a line,
#' defined by arrays of x and y coordinates, and assumed to be equidistant in time
#' if absolute = T, returns index of peak acceleration or deceleration of a line
#'
#' @export

indexMaxAcceleration <- function(x_vector, y_vector, absolute = F) {

  # distances
  distances = sqrt(
    ( ( x_vector[2:length(x_vector)] - x_vector[1:(length(x_vector)-1)] )^2) +
      ( ( y_vector[2:length(y_vector)] - y_vector[1:(length(y_vector)-1)] )^2)
  )

  # acceleration = difference in velocity = differences in distances of time-equidistant timepoints
  accelerations = distances[2:length(distances)] - distances[1:(length(distances)-1)]
  if(absolute){
    accelerations = abs(accelerations)
  }

  return(which.max(accelerations))
}
