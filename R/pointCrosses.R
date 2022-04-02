# checks how often a certain point is being crossed by an number sequence
# where the order of the array indicates timeadjacency


pointCrosses <- function(numeric_array, relevant_point = 0, filter_bool = NULL) {

  # compute array of relevant points, with boolean indicating if value is greater than relevant_point
  if(!is.null(filter_bool)) {
    if(length(numeric_array) != length(filter_bool)){
      stop("if filter_bool is supplied, it's length must equal the length of numeric_array!")
    }
    bool_a = numeric_array[filter_bool] > relevant_point
  } else {
    bool_a = numeric_array > relevant_point
  }

  # compute if consecutive bools differ, and sum up how often this happens
  bool_b = bool_a[1:length(bool_a)-1] != bool_a[2:length(bool_a)]
  out_val = sum(bool_b)

  return(rep(out_val, length(numeric_array)))
}

