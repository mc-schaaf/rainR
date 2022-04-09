#' @title pointCrosses
#'
#' @description checks how often a certain point is being crossed by an number sequence
#' where the order of the array indicates timeadjacency
#'
#' @export

pointCrosses <- function(numeric_array, relevant_point = 0) {

  # compute array of booleans indicating if value is greater than relevant_point
  bool_a = numeric_array > relevant_point

  # compute if consecutive bools differ, and sum up how often this happens
  bool_b = bool_a[1:length(bool_a)-1] != bool_a[2:length(bool_a)]

  return(sum(bool_b))

}

