#' @title Clear the global workspace
#'
#' @description Clears the global workspace but keeps certain items.
#'    If \code{keep} is not specified, it behaves exactly as the usual
#'    \code{\link[base:rm]{rm(list = ls())}}.
#'
#' @param keep Names of the items in the workspace to keep. Must be supplied as
#'    vector of strings.
#'
#' @examples
#' ## declare variables
#' a = b = c = d = e = 1
#' print(ls())
#'
#' ## remove some of the variables
#' clear_all_but(c("a", "b"))
#' print(ls())
#'
#' @export
#'

clear_all_but <- function(keep = NULL) {
  clear_vars <- ls(envir = parent.frame())
  if (!is.null(keep)) {
    clear_vars <- clear_vars[!(clear_vars %in% keep)]
  }
  rm(list = clear_vars, envir = parent.frame())
}
