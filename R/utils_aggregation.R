#' Normalize aggregation specifications
#'
#' @param aggregation_specs Optional named list or named character vector.
#'
#' @return A named character vector of aggregation rules.
#'
#' @keywords internal
#' @noRd
normalize_aggregation_specs <- function(aggregation_specs) {
  if (is.null(aggregation_specs)) {
    return(NULL)
  }

  if (is.character(aggregation_specs)) {
    if (is.null(names(aggregation_specs)) || any(!nzchar(names(aggregation_specs)))) {
      stop("`aggregation_specs` must be named.", call. = FALSE)
    }

    return(aggregation_specs)
  }

  if (!is.list(aggregation_specs)) {
    stop("`aggregation_specs` must be NULL, a named character vector, or a named list.", call. = FALSE)
  }

  spec_names <- names(aggregation_specs)

  if (is.null(spec_names) || any(!nzchar(spec_names))) {
    stop("`aggregation_specs` must be named.", call. = FALSE)
  }

  out <- vapply(
    aggregation_specs,
    function(x) {
      if (is.character(x) && length(x) == 1L) {
        return(x)
      }

      if (is.list(x) && !is.null(x$method) && is.character(x$method) && length(x$method) == 1L) {
        return(x$method)
      }

      stop(
        "`aggregation_specs` entries must be character scalars or lists with a `method` field.",
        call. = FALSE
      )
    },
    character(1)
  )

  names(out) <- spec_names

  allowed_methods <- c(
    "any",
    "all",
    "mean",
    "median",
    "sum",
    "min",
    "max",
    "first",
    "last",
    "mode",
    "paste_unique"
  )

  bad_methods <- setdiff(unname(out), allowed_methods)

  if (length(bad_methods)) {
    stop(
      "Unsupported aggregation method(s): ",
      paste(unique(bad_methods), collapse = ", "),
      ". Supported methods are: ",
      paste(allowed_methods, collapse = ", "),
      call. = FALSE
    )
  }

  out
}


#' Apply one aggregation method to one vector
#'
#' @keywords internal
#' @noRd
apply_aggregation_method <- function(x, method) {
  if (identical(method, "any")) {
    return(any(as.logical(x), na.rm = TRUE))
  }

  if (identical(method, "all")) {
    return(all(as.logical(x), na.rm = TRUE))
  }

  if (identical(method, "mean")) {
    if (all(is.na(x))) {
      return(NA_real_)
    }

    return(mean(x, na.rm = TRUE))
  }

  if (identical(method, "median")) {
    if (all(is.na(x))) {
      return(NA_real_)
    }

    return(stats::median(x, na.rm = TRUE))
  }

  if (identical(method, "sum")) {
    if (all(is.na(x))) {
      return(NA_real_)
    }

    return(sum(x, na.rm = TRUE))
  }

  if (identical(method, "min")) {
    if (all(is.na(x))) {
      return(NA_real_)
    }

    return(min(x, na.rm = TRUE))
  }

  if (identical(method, "max")) {
    if (all(is.na(x))) {
      return(NA_real_)
    }

    return(max(x, na.rm = TRUE))
  }

  if (identical(method, "first")) {
    x_non_na <- x[!is.na(x)]

    if (!length(x_non_na)) {
      return(x[NA_integer_][1])
    }

    return(x_non_na[1])
  }

  if (identical(method, "last")) {
    x_non_na <- x[!is.na(x)]

    if (!length(x_non_na)) {
      return(x[NA_integer_][1])
    }

    return(x_non_na[length(x_non_na)])
  }

  if (identical(method, "mode")) {
    x_non_na <- x[!is.na(x)]

    if (!length(x_non_na)) {
      return(x[NA_integer_][1])
    }

    ux <- unique(x_non_na)

    return(ux[which.max(tabulate(match(x_non_na, ux)))])
  }

  if (identical(method, "paste_unique")) {
    x_non_na <- unique(as.character(x[!is.na(x)]))

    if (!length(x_non_na)) {
      return(NA_character_)
    }

    return(paste(x_non_na, collapse = "; "))
  }

  stop("Unsupported aggregation method: ", method, call. = FALSE)
}