#' Convert p-values to significance stars
#'
#' Converts p-values to commonly used significance codes.
#'
#' @param p A numeric vector of p-values.
#' @param nonsig Character string to return for non-significant p-values.
#'   Defaults to `""`.
#'
#' @return A character vector containing significance codes.
#'
#' @keywords internal
format_p_stars <- function(p, nonsig = "") {

  if (!is.character(nonsig) || length(nonsig) != 1L || is.na(nonsig)) {
    stop("`nonsig` must be a single character string.", call. = FALSE)
  }

  out <- rep(nonsig, length(p))   # default
  out[p <= .05]  <- "*"
  out[p <= .01]  <- "**"
  out[p <= .001] <- "***"
  out[is.na(p)] <- NA_character_

  return(out)
}

