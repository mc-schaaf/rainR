#' Pairwise paired-samples t-tests
#'
#' Conducts all pairwise paired-samples t-tests across the levels of a
#' within-subject factor.
#' For each comparison, observations are matched using the subject identifier
#' supplied via id.
#'
#' Only complete pairs are included in each comparison.
#' Observations without two matching measurements are silently removed.
#'
#' The returned table includes descriptive statistics, the paired-samples
#' t-test result, confidence intervals, and Cohen's \eqn{d_z}.
#'
#' @param data A data frame containing the variables used in the analysis.
#' @param dv The dependent variable. Supports tidy evaluation.
#' @param within A within-subject factor defining the repeated-measures
#' conditions to be compared. Supports tidy evaluation.
#' @param id A subject identifier used to match observations across levels of
#' within. Supports tidy evaluation.
#' @param warn Logical. If TRUE (default), warnings are issued when
#' observations are removed because complete pairs are unavailable or when a
#' comparison cannot be performed due to insufficient paired observations.
#' @param ... Additional arguments passed to [stats::t.test()].
#'
#' @return
#' A tibble with one row per pairwise comparison containing:
#'
#' \describe{
#' \item{group1, group2}{The compared levels of within}
#' \item{n}{Number of paired observations included in the comparison}
#' \item{mean1, mean2}{Means of the dependent variable}
#' \item{diff}{Mean paired difference (group1 - group2)}
#' \item{se_pd}{Standard error of the paired differences}
#' \item{ci_low, ci_high}{Confidence interval for the mean paired difference}
#' \item{df}{Degrees of freedom}
#' \item{t}{t statistic}
#' \item{p}{p-value}
#' \item{dz}{Cohen's \eqn{d_z}}
#' \item{p_str}{Significance stars based on the p-value}
#' }
#'
#'
#' @export
pairwise_paired_t_test <- function(data, dv, within, id, warn = TRUE, ...) {

  ## capture columns for later tidyverse style injection
  dv     <- rlang::ensym(dv)
  within <- rlang::ensym(within)
  id     <- rlang::ensym(id)

  within_col <- rlang::as_string(within)
  id_col <- rlang::as_string(id)


  ## input validation
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.logical(warn) || length(warn) != 1 || is.na(warn)) {
    stop("`warn` must be TRUE or FALSE.", call. = FALSE)
  }

  required_cols <- c(
    rlang::as_string(dv),
    rlang::as_string(within),
    rlang::as_string(id)
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste0("Missing column(s): ",paste(missing_cols,collapse = ", ")), call. = FALSE)
  }


  tab <- table(data[[id_col]], data[[within_col]])
  if (any(tab > 1)) {
    stop("Each id-within combination must occur only once.", call. = FALSE)
  }


  within_levels <- data |>
    dplyr::pull(!!within) |>
    unique()

  if (length(within_levels) < 2) {
    stop("`within` must contain at least two levels.", call. = FALSE)
  }


  ## actual computation
  all_comparisons <- utils::combn(within_levels, 2, simplify = FALSE)

  results <- vector(mode = "list",length = length(all_comparisons))

  for (i in seq_along(all_comparisons)) {

    comparison <- all_comparisons[[i]]

    d1 <- data[data[[within_col]] == comparison[1], , drop = FALSE]
    d2 <- data[data[[within_col]] == comparison[2], , drop = FALSE]

    merged <- dplyr::inner_join(
      d1[, c(id_col, rlang::as_string(dv))],
      d2[, c(id_col, rlang::as_string(dv))],
      by = id_col,
      suffix = c("_1", "_2")
    )

    if (nrow(merged) < 2) {
      if (warn) {
        warning(
          paste0("Skipping comparison ", comparison[1]," vs ",comparison[2],
                 " because fewer than 2 complete pairs were available."),
          call. = FALSE
        )
      }
      next
    }

    if (warn && (nrow(d1) - nrow(merged) > 0 || nrow(d2) - nrow(merged) > 0)) {
      warning(paste0("Comparison ", comparison[1], " vs ", comparison[2],
                     ": removed ", nrow(d1) - nrow(merged),
                     " observation(s) from group 1 and ", nrow(d2) - nrow(merged),
                     " observation(s) from group 2 because complete pairs were required."),
              call. = FALSE
      )
    }

    val1 <- merged[[paste0(rlang::as_string(dv), "_1")]]
    val2 <- merged[[paste0(rlang::as_string(dv), "_2")]]
    diff <- val1 - val2

    t_test <- stats::t.test(val1, val2, paired = TRUE, ...)

    # append to results list
    results[[i]] <- tibble::tibble(
      group1  = as.character(comparison[1]),
      group2  = as.character(comparison[2]),
      n       = nrow(merged),
      mean1   = mean(val1, na.rm=TRUE),
      mean2   = mean(val2, na.rm=TRUE),
      diff    = mean(diff, na.rm=TRUE),
      se_pd   = stats::sd(diff, na.rm=TRUE) / sqrt(nrow(merged)),
      ci_low  = unname(t_test$conf.int[1]),
      ci_high = unname(t_test$conf.int[2]),
      df      = unname(t_test$parameter),
      t       = unname(t_test$statistic),
      p       = t_test$p.value,
      dz      = unname(t_test$statistic) / sqrt(nrow(merged))
    )
  }

  # convert list to single data.frame and return
  return(dplyr::bind_rows(results))
}








#' Grouped pairwise paired-samples t-tests
#'
#' Runs \code{pairwise_paired_t_test()} within groups defined by one or more variables.
#'
#' @inheritParams pairwise_paired_t_test
#' @param by Grouping variables (tidy-evaluated; supports multiple columns).
#'
#' @return A tibble with results split by grouping variables.
#'
#' @export
pairwise_paired_t_test_grouped <- function(data, dv, within, id, by, warn = TRUE, ...) {

  ## capture columns for later tidyverse style injection
  dv     <- rlang::ensym(dv)
  within <- rlang::ensym(within)
  id     <- rlang::ensym(id)
  by     <- rlang::enquos(by)

  ## input validation
  if (length(by) == 0) {
    stop("`by` must specify at least one grouping variable.", call. = FALSE)
  }

  missing_by <- setdiff(vapply(by, rlang::as_name, character(1)), names(data))

  if (length(missing_by) > 0) {
    stop(paste0("Grouping variable(s) not found in data: ", paste(missing_by, collapse = ", ")), call. = FALSE)
  }

  ## grouped computation
  dplyr::group_by(data, !!!by) |>
    dplyr::group_modify(function(.x, .y) {
      res <- pairwise_paired_t_test(
        data   = .x,
        dv     = !!dv,
        within = !!within,
        id     = !!id,
        warn   = warn,
        ...
      )
      # attach grouping columns
      return(dplyr::bind_cols(dplyr::as_tibble(.y), res))
    }) |>
    dplyr::ungroup()
}


