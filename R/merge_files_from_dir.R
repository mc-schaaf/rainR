#' @title Merges Files of the Same Format
#'
#' @description crawls the supplied `dir_path` and searches for files matching
#' `regex_exp`. It then reads in those files and merges the data into one data
#' frame via appending. Hence, files must have the same data format
#' (number and identity of columns).
#'
#' @param regex_exp A regular expression.
#' Only file names which match the regular expression will be returned.
#'
#' @param dir_path a character vector of full path names;
#' the default corresponds to the working directory, \code{\link[base]{getwd}}.
#' Tilde expansion (see \code{\link[base]{path.expand}}) is performed.
#' Missing values will be ignored.
#' Elements with a marked encoding will be converted to the native encoding
#' (and if that fails, considered non-existent).
#' See \code{\link[base]{list.files}}.
#'
#' @param debug debug options. Must be one of "no", "return.names", or
#' "print.bad.files". Defaults to "no".
#'
#' @param include_file_information whether to include some path information to
#' the returned data set.
#'
#' @param ... additional arguments that will be supplied to the underlying
#' \code{\link[data.table]{fread}} function
#'
#' @return merged dataset
#'
#' @export
#' @importFrom data.table "fread"
#'

merge_files_from_dir <- function(regex_exp,
                                 dir_path = NULL,
                                 debug = "no",
                                 include_file_information = FALSE,
                                 ...) {

    if (is.null(dir_path)) {
      dir_path = getwd()
    }

    file_list <- list.files(
      path = dir_path,
      pattern = regex_exp,
      full.names = FALSE,
      recursive = TRUE,
      no.. = TRUE
    )

    if (!(debug %in% c("no","return.names","print.bad.files"))) {
      warning("debug parameter is wrong. Must be one of \"no\", \"return.names\", or \"print.bad.files\".")
    }
    if (debug == "return.names") {
      return(file_list)
    }
    if (debug == "print.bad.files") {
      for (file in file_list) {
        tryCatch(data.table::fread(file, ...),
                 warning = function(w){
                   print(file)
                   print(w)
                 })
      }
      return(NA)
    }

    if (include_file_information) {
      for (file in file_list) {
        if (exists("dataset")) {
          temp_dataset <-  data.table::fread(file, ...)
          temp_dataset$fileName <- paste(file, sep = "")
          temp_dataset$path <- paste(getwd(), sep = "")
          dataset <- rbind(dataset, temp_dataset, use.names = FALSE)
        }
        if (!exists("dataset")) {
          dataset <- data.table::fread(file, ...)
          dataset$fileName <- paste(file, sep = "")
          dataset$path <- paste(getwd(), sep = "")
        }
      }
    } else {
      for (file in file_list) {
        if (exists("dataset")) {
          temp_dataset <-  data.table::fread(file, ...)
          dataset <- rbind(dataset, temp_dataset, use.names = FALSE)
        }
        if (!exists("dataset")) {
          dataset <- data.table::fread(file, ...)
        }
      }
    }

    return(dataset)
  }
