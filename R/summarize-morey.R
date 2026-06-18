#' Compute Morey-corrected standard error of a single cell
#'
#' Calculates the within-subject standard error of a single design cell using
#' the correction proposed by Morey (2008).
#' The function first computes the standard deviation of participant-normalized
#' scores and then applies the Morey correction factor \eqn{\sqrt{k/(k-1)}}.
#'
#' @param x Numeric vector of observations.
#' @param id_mean Numeric vector of participant means corresponding
#'   to each observation in `x`.
#' @param k Integer giving the number of within-subject conditions.
#'
#' @return A single numeric value giving the Morey-corrected standard
#'   error.
#'
#' @keywords internal
#' @noRd
morey_se <- function(x, id_mean, k) {

  stopifnot(length(x) == length(id_mean))

  uncorrected_se <- stats::sd(x-id_mean, na.rm = FALSE) / sqrt(length(x))
  return(uncorrected_se * sqrt(k/(k-1)))
}




#' Extract variable names from a quosure
#'
#' Parses a quosure supplied to a user-facing function and returns the
#' referenced variable names. Supports either a single variable
#' (`var`) or multiple variables supplied via `c(var1, var2, ...)`.
#'
#' @param x A quosure captured with [rlang::enquo()].
#'
#' @return A character vector of variable names.
#'
#' @keywords internal
#' @noRd
parse_var_names <- function(x) {

  expr <- rlang::get_expr(x)

  if (rlang::is_symbol(expr)) {
    return(rlang::as_name(expr))
  }
  if (rlang::is_call(expr, "c")) {
    return(vapply(as.list(expr)[-1], rlang::as_name, character(1)))
  }
  stop(
    "Variables must be supplied as `var` or `c(var1, var2, ...)`.",
    call. = FALSE
  )
}




#' Validate and perpare inputs for Morey summaries
#'
#' Performs input validation and preprocessing for
#' [summarize_morey()]. Checks that required variables exist,
#' dependent variables are numeric, participant identifiers are
#' complete, and within-subject factors are stored as factors.
#' Unused factor levels may be dropped and duplicate participant-by-cell
#' observations are aggregated by their mean.
#'
#' @param data A data frame containing the study data.
#' @param dv_names Character vector of dependent variable names.
#' @param iv_names Character vector of within-subject factor names.
#' @param id_name Character string naming the participant identifier
#'   variable.
#' @param warn Logical indicating whether warnings should be issued.
#'
#' @return A validated and possibly modified data frame.
#'
#' @keywords internal
#' @noRd
prepare_morey_data <- function(data, dv_names, iv_names, id_name, warn = TRUE) {

  # basic checks
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.logical(warn) || length(warn) != 1 || is.na(warn)) {
    stop("`warn` must be TRUE or FALSE.", call. = FALSE)
  }

  # variable existence check
  all_vars <- c(id_name, dv_names, iv_names)

  missing_vars <- setdiff(all_vars, names(data))

  if (length(missing_vars) > 0) {
    stop(
      paste0("These variables are not in `data`: ", paste(missing_vars, collapse = ", ")),
      call. = FALSE
    )
  }

  # dvs must be numeric
  for (v in dv_names) {
    if (!is.numeric(data[[v]])) {
      stop(paste0("`", v, "` must be numeric."), call. = FALSE)
    }
  }

  # id check
  if (anyNA(data[[id_name]])) {
    stop("`id` contains missing values.", call. = FALSE)
  }

  # ensure within-factors are factors
  for (v in iv_names) {
    if (!is.factor(data[[v]])) {

      if (warn) {
        warning(paste0("Converting `", v, "` to factor."), call. = FALSE)
      }

      data[[v]] <- factor(data[[v]])
    }

    # drop unused levels
    old_levels <- levels(data[[v]])
    data[[v]] <- droplevels(data[[v]])
    new_levels <- levels(data[[v]])

    if (warn && length(old_levels) != length(new_levels)) {
      warning(
        paste0(
          "Dropped unused factor levels in `", v,
          "`: ", paste(setdiff(old_levels, new_levels), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  # check for duplicate rows
  dup_check <- data |>
    dplyr::count(dplyr::across(dplyr::all_of(c(id_name, iv_names)))) |>
    dplyr::filter(.data$n > 1)

  if (nrow(dup_check) > 0) {
    if (warn) {
      warning(
        "Some id x condition cells have multiple rows. Means will be taken over them.",
        call. = FALSE
      )
    }
    data <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(id_name, iv_names)))) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(dv_names), ~ mean(.x, na.rm = FALSE)),
        .groups = "drop"
      )
  }

  return(data)
}




#' Validate repeated-measures design assumptions
#'
#' Checks that the data satisfy the requirements for computing
#' Morey-corrected standard errors. Rows containing missing values are
#' removed, the set of observed within-subject conditions is identified,
#' and participant coverage across conditions is verified.
#'
#' The function determines the number of observed within-subject cells
#' (`k`) used in the Morey correction and ensures that every participant
#' contributes data to all observed cells.
#'
#' @param data A validated data frame.
#' @param dv_names Character vector of dependent variable names.
#' @param iv_names Character vector of within-subject factor names.
#' @param id_name Character string naming the participant identifier
#'   variable.
#' @param warn Logical indicating whether warnings should be issued.
#'
#' @return An integer giving the number of observed within-subject
#'   conditions (`k`).
#'
#' @keywords internal
#' @noRd
validate_morey_design <- function(data, dv_names, iv_names, id_name, warn = TRUE) {

  # remove rows where any values are missing
  n_before <- nrow(data)
  data <- data |>
    dplyr::filter(
      !is.na(.data[[id_name]]),
      dplyr::if_all(dplyr::all_of(iv_names), ~ !is.na(.x)),
      dplyr::if_all(dplyr::all_of(dv_names), ~ !is.na(.x))
    )

  if (warn && n_before != nrow(data)) {
    warning(paste0("Dropped ", n_before-nrow(data), " rows due to missing values."))
  }

  # observed within-subject cells
  observed_cells <- data |>
    dplyr::distinct(dplyr::across(dplyr::all_of(iv_names)))

  k <- nrow(observed_cells)

  if (k <= 1) {
    stop("Need at least 2 within-subject cells for Morey correction.", call. = FALSE)
  }

  # detect missing IV combinations
  iv_levels <- lapply(iv_names, function(v) {
    unique(data[[v]])
  })
  names(iv_levels) <- iv_names

  full_cells <- expand.grid(iv_levels, stringsAsFactors = FALSE)

  missing_cells <- dplyr::anti_join(
    full_cells,
    observed_cells,
    by = iv_names
  )

  if (warn && nrow(missing_cells) > 0) {
    warning(
      paste0(nrow(missing_cells), " IV combinations are absent from the data."),
      call. = FALSE
    )
  }

  # check full id x cell coverage
  subject_cells <- data |>
    dplyr::count(dplyr::across(dplyr::all_of(c(id_name, iv_names))))

  incomplete <- subject_cells |>
    dplyr::count(.data[[id_name]]) |>
    dplyr::filter(.data$n != k)

  if (nrow(incomplete) > 0) {
    stop(
      paste0(nrow(incomplete)," participants do not contribute to all ", k, " cells."),
      call. = FALSE
    )
  }

  return(k)
}




#' Summarize repeated-measures data with Morey-corrected standard errors
#'
#' Computes condition means and within-subject standard errors using the
#' correction proposed by Morey (2008) for repeated-measures designs.
#' One or more dependent variables can be summarized across one or more
#' within-subject factors.
#'
#' The function expects a fully within-subject design in which each
#' participant contributes an observation to every observed condition
#' combination. If duplicate observations are present within a
#' participant-by-condition cell, they are averaged before summary
#' statistics are computed.
#'
#' @param data A data frame containing the observations.
#' @param dvs One or more dependent variables specified as bare column
#'   names. Multiple variables can be supplied using
#'   `c(var1, var2, ...)`.
#' @param within One or more within-subject factors specified as bare
#'   column names. Multiple factors can be supplied using
#'   `c(factor1, factor2, ...)`.
#' @param id Participant identifier specified as a bare column name.
#' @param warn Logical indicating whether warnings should be emitted for
#'   non-fatal issues such as factor conversion, dropped factor levels,
#'   duplicate observations, or removed missing values.
#'
#' @return A tibble containing one row per condition combination and
#'   dependent variable. The output includes:
#'
#'   * the within-subject factor columns,
#'   * `dv`: the dependent variable name,
#'   * `mean`: the condition mean,
#'   * `se`: the Morey-corrected standard error,
#'   * `n`: the number of participants contributing to the condition after
#'     removing missing values,
#'   * `k`: the number of within-subject conditions used in the
#'     correction.
#'
#' @references
#' Morey, R. D. (2008). Confidence intervals from normalized data:
#' A correction to Cousineau (2005). *Tutorials in Quantitative Methods
#' for Psychology, 4*(2), 61-64. https://doi.org/10.20982/tqmp.04.2.p061
#'
#' @export
#' @importFrom tidyr pivot_longer
summarize_morey <- function(data, dvs, within, id, warn = TRUE) {

  # capture user input
  dv_quo <- rlang::enquo(dvs)
  iv_quo <- rlang::enquo(within)
  id_quo <- rlang::enquo(id)

  dv_names <- parse_var_names(dv_quo)
  iv_names <- parse_var_names(iv_quo)
  id_name  <- rlang::as_name(id_quo)


  # validate data and design and extract number of distinct conditions
  data <- prepare_morey_data(data, dv_names, iv_names, id_name, warn)
  k <- validate_morey_design(data, dv_names, iv_names, id_name, warn)

  # cast to long format to easily support multiple DVs
  long_data <- data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(dv_names),
      names_to = "dv",
      values_to = "value"
    )

  id_means <- long_data |>
    dplyr::group_by(!!sym(id_name), .data$dv) |>
    dplyr::summarise(
      id_mean = mean(.data$value, na.rm = FALSE),
      .groups = "drop"
    )

  long_data <- dplyr::left_join(
    long_data,
    id_means,
    by = c(id_name, "dv")
  )

  out <- long_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(iv_names)), .data$dv) |>
    dplyr::summarise(
      mean = mean(.data$value, na.rm = FALSE),
      se   = morey_se(.data$value, .data$id_mean, k),
      n    = dplyr::n(),
      k    = k,
      .groups = "drop"
    )

  return(out)
}



