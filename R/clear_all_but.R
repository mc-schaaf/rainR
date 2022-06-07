#' @title Clear the workspace
#'
#' @description Clears the workspace but keeps certain items. If 'keep' is not
#' specified, behaves exactly as the usual 'rm(list = ls())'.
#'
#' @param keep Names of the items in the workspace to keep. Must be supplied as
#' vector of strings.
#'
#' @examples
#' a = b = c = d = e = 1
#' print(ls())
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
