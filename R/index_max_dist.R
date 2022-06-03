#' @title index_max_dist
#'
#' @description computes the index of the peak velocity of a trajectory,
#' defined by arrays of x and y coordinates, and assumed to be equidistant in time
#'
#' @param x_vector vector of the x-coordinates of the executed trajectory
#' @param y_vector vector of the y-coordinates of the executed trajectory
#'
#' @return single number indicating the index of peak velocity
#'
#' @examples
#'
#' data("dat_one_trajectory")
#' index_max_dist(dat_one_trajectory$xvals, dat_one_trajectory$yvals)
#'
#' @export
#'

index_max_dist <- function(x_vector, y_vector) {
  # distances
  ds_real <- sqrt(((x_vector[2:length(x_vector)] - x_vector[1:(length(x_vector) - 1)]) ^ 2) +
                   ((y_vector[2:length(y_vector)] - y_vector[1:(length(y_vector) - 1)]) ^ 2))

  return(which.max(ds_real))

}
