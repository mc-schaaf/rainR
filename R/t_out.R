#' @title Format t-Test Output
#'
#' @description Distilles the most relevant data from an output object of
#' \code{t.test} and displays the results in a compact format. Wrapper to
#' \code{\link[schoRsch]{t_out}}, but includes absolute differences.
#'
#' @param toutput Output object created by a call to \code{t.test}.
#' @param n.equal Only applicable to two-sample t-tests. If sample sizes are
#' not equal, \code{n.equal} specifies a vector of sample sizes,
#' e.g., \code{n.equal = c(12,8)}.
#' @param welch.df.exact Only applicable to Welch-tests. Indicates whether
#' Welch-adjusted or unadjusted degrees of freedom (dfs) are reported
#' (default=\code{TRUE}, i.e., Welch-adjusted dfs). If set to \code{FALSE},
#' the parameter \code{welch.n} has to be set as well.
#' @param welch.n Only applicable to Welch-tests with unadjusted degrees of
#' freedom. Parameter should be equal to the total sample
#' size \code{n=n_1+n_2}.
#' @param d.corr Only applicable to one-sample or paired-samples t-tests.
#' If \code{TRUE}, Cohen's ds are computed using \code{sqrt(2)}-correction.
#' Default changed to \code{FALSE} from version 1.5 onwards with an additional
#' feedback message showing the use of corrections.
#' @param print Force results to be displayed, even if the function output is
#' assigned to a variable (e.g., \code{output <- t_out(...)};
#' logical; default=\code{TRUE}).
#' @param abs.diff Specifies the decimal places the absolute difference should
#' be rounded to. If \code{NULL}, does not display absolute differences
#' (APA 7th Standard).
#'
#' @return returns a list containing (1) a description of the t-test
#' (two-sample t-test, Welch-test, paired-samples t-test, one-sample t-test)
#' and (2) a line with formatted results.
#'
#' @details The output of a call to \code{t_out} is formatted according to the
#' guidelines of the APA (American Psychological Association) as well as the
#' DGPs ("Deutsche Gesellschaft fuer Psychologie"; German Psychological Society).
#'
#' @export
#' @importFrom schoRsch "t_out"
#' @importFrom utils "capture.output"
#'

t_out <- function(toutput,
                    n.equal = TRUE,
                    welch.df.exact = TRUE,
                    welch.n = NA,
                    d.corr = FALSE,
                    print = TRUE,
                    abs.diff = NULL) {

  # get result table from schoRsch package
  schorsch_out <- schoRsch::t_out(
    toutput = toutput,
    n.equal = n.equal,
    welch.df.exact = welch.df.exact,
    welch.n = welch.n,
    d.corr = d.corr,
    print = FALSE
  )

  # get Note from schoRsch package
  schorsch_string <- capture.output(
    schoRsch::t_out(
      toutput = toutput,
      n.equal = n.equal,
      welch.df.exact = welch.df.exact,
      welch.n = welch.n,
      d.corr = d.corr,
      print = TRUE
    )
  )
  note = schorsch_string[grep("NOTE:", schorsch_string)]

  # add the absolute difference to the output
  if (!is.null(abs.diff)) {
    schorsch_out$Results = paste0(schorsch_out$Results,
                                  ", delta = ",
                                  round(toutput$estimate, abs.diff))
  }

  # print output, if wanted
  if (print == TRUE) {
    print(schorsch_out)
    if (length(note)>0) {
      cat(paste0("\n", note, "\n"))
    }
  }

  # return an invisible version of the out table
  return(invisible(schorsch_out))

}
