#' @title index_max_acceleration
#'
#' @description computes the index of the peak acceleration of a trajectory,
#' defined by arrays of x and y coordinates, and assumed to be equidistant in time
#'
#' @param x_vector vector of the x-coordinates of the executed trajectory
#' @param y_vector vector of the y-coordinates of the executed trajectory
#' @param absolute should negative accelerations (i.e., deceleration) be included?
#'
#' @return single number indicating the number of peak acceleration
#'
#' @examples
#'
#' data("dat_one_trajectory")
#' index_max_acceleration(dat_one_trajectory$xvals, dat_one_trajectory$yvals)
#'
#' @export
#'

index_max_acceleration <- function(x_vector, y_vector, absolute = FALSE) {

    # distances
    distances <- sqrt(((x_vector[2:length(x_vector)] - x_vector[1:(length(x_vector) - 1)]) ^ 2) +
                       ((y_vector[2:length(y_vector)] - y_vector[1:(length(y_vector) - 1)]) ^ 2))

    # acceleration = difference in velocity = differences in distances of time-equidistant timepoints
    accelerations <- distances[2:length(distances)] -
      distances[1:(length(distances) - 1)]

    if (absolute) {
      accelerations <- abs(accelerations)
    }

    return(which.max(accelerations))
  }
