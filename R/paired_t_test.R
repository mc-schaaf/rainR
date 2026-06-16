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

  duplicates <- data |>
    dplyr::count(!!id, !!within) |>
    dplyr::filter(n > 1)

  if (nrow(duplicates) > 0) {
    stop("Each id-within combination must occur only once.", call. = FALSE)
  }

  within_levels <- data |>
    dplyr::pull(!!within) |>
    unique()

  if (length(within_levels) < 2) {
    stop("`within` must contain at least two levels.", call. = FALSE)
  }


  ## actual computation
  all_comparisons <- combn(within_levels, 2, simplify = FALSE)

  results <- vector(mode = "list",length = length(all_comparisons))

  for (i in seq_along(all_comparisons)) {

    comparison <- all_comparisons[[i]]

    d1 <- data |> dplyr::filter(!!within == comparison[1])
    d2 <- data |> dplyr::filter(!!within == comparison[2])

    merged <- dplyr::inner_join(
      d1 |> dplyr::select(!!id, val1 = !!dv),
      d2 |> dplyr::select(!!id, val2 = !!dv),
      by = rlang::as_string(id)
    )

    n_pairs <- nrow(merged)

    if (n_pairs < 2) {
      if (warn) {
        warning(
          paste0("Skipping comparison ", comparison[1]," vs ",comparison[2],
                 " because fewer than 2 complete pairs were available."),
          call. = FALSE
        )
      }
      next
    }

    dropped1 <- nrow(d1) - n_pairs
    dropped2 <- nrow(d2) - n_pairs

    if (warn && (dropped1 > 0 || dropped2 > 0)) {
      warning(paste0("Comparison ", comparison[1], " vs ", comparison[2], ": removed ",
                     dropped1, " observation(s) from group 1 and ", dropped2,
                     " observation(s) from group 2 because complete pairs were required."),
              call. = FALSE
      )
    }

    merged$diff <- merged$val1 - merged$val2

    t_test <- stats::t.test(merged$val1, merged$val2, paired = TRUE, ...)

    # append to results list
    results[[i]] <- tibble::tibble(
      group1  = as.character(comparison[1]),
      group2  = as.character(comparison[2]),
      n       = n_pairs,
      mean1   = mean(merged$val1, na.rm=TRUE),
      mean2   = mean(merged$val2, na.rm=TRUE),
      diff    = mean(merged$diff, na.rm=TRUE),
      se_pd   = sd(merged$diff, na.rm=TRUE) / sqrt(n_pairs),
      ci_low  = unname(t_test$conf.int[1]),
      ci_high = unname(t_test$conf.int[2]),
      df      = unname(t_test$parameter),
      t       = unname(t_test$statistic),
      p       = t_test$p.value,
      dz      = unname(t_test$statistic) / sqrt(n_pairs)
    )
  }

  # convert list to single data.frame
  results <- dplyr::bind_rows(results)

  results <- results |>
    dplyr::mutate(
      p_str = p_stars(p)
    )

  return(results)
}
