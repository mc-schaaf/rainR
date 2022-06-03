#' @title point_crosses
#'
#' @description checks how often a certain point is being crossed by an number sequence
#' where the order of the array indicates timeadjacency
#'
#' @param numeric_array array of numbers ordered by their time of appearance
#' @param relevant_point number which has to be crossed
#'
#' @return number of times that numeric_array crosses the relevant_point
#'
#' @examples
#'
#' data("dat_one_trajectory")
#' point_crosses(dat_one_trajectory$xvals)
#'
#' @export
#'

point_crosses <- function(numeric_array, relevant_point = 0) {
  if (length(numeric_array) < 2) {
    warning("Less than two values supplied!")
    return(0)
  }

  # compute array of booleans indicating if value is greater than relevant_point
  bool_a <- numeric_array > relevant_point

  # compute if consecutive bools differ, and sum up how often this happens
  bool_b <- bool_a[1:(length(bool_a) - 1)] != bool_a[2:length(bool_a)]

  return(sum(bool_b))

}
