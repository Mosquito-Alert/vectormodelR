#' Create a scaling specification
#'
#' Creates a small list describing how a raw input column should be transformed
#' and scaled into a model-ready output column.
#'
#' @param input Character scalar. Name of the raw input column.
#' @param output Character scalar. Name of the scaled output column to create.
#' @param transform Character scalar or function. Transformation to apply before
#'   scaling. Supported character values are `"identity"`, `"log1p"`, `"log"`,
#'   and `"sqrt"`. A custom function can also be supplied.
#' @param scale_name Character scalar. Name used to store the scaling parameters.
#'   Defaults to `input`.
#'
#' @return A list with fields `input`, `output`, `transform`, and `scale_name`.
#'
#' @keywords internal
#' @noRd
scale_spec <- function(
    input,
    output,
    transform = "identity",
    scale_name = input
) {
  if (!is.character(input) || length(input) != 1L || !nzchar(input)) {
    stop("`input` must be a single non-empty character value.", call. = FALSE)
  }

  if (!is.character(output) || length(output) != 1L || !nzchar(output)) {
    stop("`output` must be a single non-empty character value.", call. = FALSE)
  }

  if (!is.character(scale_name) || length(scale_name) != 1L || !nzchar(scale_name)) {
    stop("`scale_name` must be a single non-empty character value.", call. = FALSE)
  }

  if (!is.character(transform) && !is.function(transform)) {
    stop("`transform` must be a character value or a function.", call. = FALSE)
  }

  if (is.character(transform) && (length(transform) != 1L || !nzchar(transform))) {
    stop("`transform` must be a single non-empty character value.", call. = FALSE)
  }

  list(
    input = input,
    output = output,
    transform = transform,
    scale_name = scale_name
  )
}


#' Validate a scaling specification
#'
#' Checks that a scaling specification has the required structure.
#'
#' @param spec A list created by [scale_spec()] or with equivalent fields.
#' @param spec_name Character scalar. Name of the specification, used in error
#'   messages.
#'
#' @return Invisibly returns `TRUE` if valid.
#'
#' @keywords internal
#' @noRd
validate_scale_spec <- function(spec, spec_name = "unnamed") {
  if (!is.list(spec)) {
    stop("Scale spec `", spec_name, "` must be a list.", call. = FALSE)
  }

  required_fields <- c("input", "output")

  missing_fields <- setdiff(required_fields, names(spec))

  if (length(missing_fields)) {
    stop(
      "Scale spec `", spec_name, "` is missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.character(spec$input) || length(spec$input) != 1L || !nzchar(spec$input)) {
    stop(
      "Scale spec `", spec_name, "` must have a single non-empty character `input`.",
      call. = FALSE
    )
  }

  if (!is.character(spec$output) || length(spec$output) != 1L || !nzchar(spec$output)) {
    stop(
      "Scale spec `", spec_name, "` must have a single non-empty character `output`.",
      call. = FALSE
    )
  }

  if (!is.null(spec$scale_name) &&
      (!is.character(spec$scale_name) ||
       length(spec$scale_name) != 1L ||
       !nzchar(spec$scale_name))) {
    stop(
      "Scale spec `", spec_name, "` must have a single non-empty character `scale_name`.",
      call. = FALSE
    )
  }

  if (!is.null(spec$transform) &&
      !is.character(spec$transform) &&
      !is.function(spec$transform)) {
    stop(
      "Scale spec `", spec_name, "` must have `transform` as a character value or function.",
      call. = FALSE
    )
  }

  if (is.character(spec$transform) &&
      (length(spec$transform) != 1L || !nzchar(spec$transform))) {
    stop(
      "Scale spec `", spec_name, "` must have a single non-empty character `transform`.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Validate a list of scaling specifications
#'
#' Checks that `scale_specs` is a named list and that every element is a valid
#' scaling specification.
#'
#' @param scale_specs A named list of scaling specifications.
#'
#' @return Invisibly returns `TRUE` if valid.
#'
#' @keywords internal
#' @noRd
validate_scale_specs <- function(scale_specs) {
  if (!is.list(scale_specs)) {
    stop("`scale_specs` must be NULL or a named list.", call. = FALSE)
  }

  spec_names <- names(scale_specs)

  if (is.null(spec_names) || any(!nzchar(spec_names))) {
    stop("`scale_specs` must be a named list.", call. = FALSE)
  }

  duplicated_names <- spec_names[duplicated(spec_names)]

  if (length(duplicated_names)) {
    stop(
      "`scale_specs` contains duplicated names: ",
      paste(unique(duplicated_names), collapse = ", "),
      call. = FALSE
    )
  }

  for (spec_name in spec_names) {
    validate_scale_spec(scale_specs[[spec_name]], spec_name)
  }

  invisible(TRUE)
}


#' Apply a scaling transform
#'
#' Applies a transformation before standardization.
#'
#' @param x Numeric vector.
#' @param transform Character scalar or function. Supported character values are
#'   `"identity"`, `"log1p"`, `"log"`, and `"sqrt"`. A custom function can also
#'   be supplied.
#'
#' @return Transformed numeric vector.
#'
#' @keywords internal
#' @noRd
apply_scale_transform <- function(x, transform = "identity") {
  if (is.null(transform) || identical(transform, "identity")) {
    return(x)
  }

  if (identical(transform, "log1p")) {
    if (any(x < -1, na.rm = TRUE)) {
      stop("`log1p` transform cannot be applied to values less than -1.", call. = FALSE)
    }

    return(log1p(x))
  }

  if (identical(transform, "log")) {
    if (any(x <= 0, na.rm = TRUE)) {
      stop("`log` transform cannot be applied to values less than or equal to 0.", call. = FALSE)
    }

    return(log(x))
  }

  if (identical(transform, "sqrt")) {
    if (any(x < 0, na.rm = TRUE)) {
      stop("`sqrt` transform cannot be applied to negative values.", call. = FALSE)
    }

    return(sqrt(x))
  }

  if (is.function(transform)) {
    transformed <- transform(x)

    if (length(transformed) != length(x)) {
      stop("Custom transform function must return a vector of the same length as `x`.", call. = FALSE)
    }

    return(transformed)
  }

  stop(
    "`transform` must be one of 'identity', 'log1p', 'log', 'sqrt', or a function.",
    call. = FALSE
  )
}


#' Scale a numeric vector and return parameters
#'
#' Centers and scales a numeric vector using its mean and standard deviation.
#'
#' @param x Numeric vector.
#' @param name Character scalar. Name used in warnings and scaling metadata.
#'
#' @return A list with:
#'   \item{values}{Scaled values.}
#'   \item{params}{List containing `mean` and `sd`.}
#'
#' @keywords internal
#' @noRd
scale_numeric_vector <- function(x, name = "x") {
  if (!is.numeric(x)) {
    stop("Variable `", name, "` must be numeric to be scaled.", call. = FALSE)
  }

  mu <- mean(x, na.rm = TRUE)
  sigma <- stats::sd(x, na.rm = TRUE)

  params <- list(
    mean = mu,
    sd = sigma
  )

  if (is.nan(mu)) {
    warning(
      "Variable `", name, "` has no non-missing values; scaled values set to NA.",
      call. = FALSE
    )

    return(list(
      values = rep(NA_real_, length(x)),
      params = params
    ))
  }

  if (is.na(sigma) || sigma == 0) {
    warning(
      "Variable `", name, "` has zero or undefined standard deviation; scaled values set to 0.",
      call. = FALSE
    )

    return(list(
      values = rep(0, length(x)),
      params = params
    ))
  }

  list(
    values = (x - mu) / sigma,
    params = params
  )
}


#' Apply scaling specifications to a data frame
#'
#' Applies a named list of scaling specifications to a data frame, creating
#' scaled output columns and returning the scaling parameters.
#'
#' @param df Data frame.
#' @param scale_specs Named list of scaling specifications.
#'
#' @return A list with:
#'   \item{data}{Data frame with scaled columns added.}
#'   \item{scaling}{List of scaling parameters.}
#'
#' @keywords internal
#' @noRd
apply_scale_specs <- function(df, scale_specs) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  if (is.null(scale_specs)) {
    return(list(
      data = df,
      scaling = list()
    ))
  }

  validate_scale_specs(scale_specs)

  scaling <- list()

  for (spec_name in names(scale_specs)) {
    spec <- scale_specs[[spec_name]]

    input_col <- spec$input
    output_col <- spec$output
    transform <- if (!is.null(spec$transform)) spec$transform else "identity"
    scale_name <- if (!is.null(spec$scale_name)) spec$scale_name else spec_name

    if (!input_col %in% names(df)) {
      stop(
        "Scale spec `", spec_name, "` uses input column `", input_col,
        "`, but that column is not in the dataset.",
        call. = FALSE
      )
    }

    x <- df[[input_col]]

    if (!is.numeric(x)) {
      stop(
        "Scale spec `", spec_name, "` uses input column `", input_col,
        "`, but that column is not numeric.",
        call. = FALSE
      )
    }

    transformed_x <- apply_scale_transform(x, transform)

    scaled <- scale_numeric_vector(transformed_x, scale_name)

    df[[output_col]] <- scaled$values
    scaling[[scale_name]] <- scaled$params

    scaling[[scale_name]]$input <- input_col
    scaling[[scale_name]]$output <- output_col
    scaling[[scale_name]]$transform <- if (is.function(transform)) {
      "custom_function"
    } else {
      transform
    }
  }

  list(
    data = df,
    scaling = scaling
  )
}

#' Subset scaling specifications by input column
#'
#' Convenience helper for keeping only specs whose input column appears in a
#' supplied character vector.
#'
#' @param scale_specs Named list of scaling specifications.
#' @param inputs Character vector of input column names to keep.
#'
#' @return A named list of scaling specifications.
#'
#' @keywords internal
#' @noRd
subset_scale_specs_by_input <- function(scale_specs, inputs) {
  if (is.null(scale_specs)) {
    return(NULL)
  }

  validate_scale_specs(scale_specs)

  if (!is.character(inputs)) {
    stop("`inputs` must be a character vector.", call. = FALSE)
  }

  keep <- vapply(
    scale_specs,
    function(spec) spec$input %in% inputs,
    logical(1)
  )

  scale_specs[keep]
}


#' Subset scaling specifications by output column
#'
#' Convenience helper for keeping only specs whose output column appears in a
#' supplied character vector.
#'
#' @param scale_specs Named list of scaling specifications.
#' @param outputs Character vector of output column names to keep.
#'
#' @return A named list of scaling specifications.
#'
#' @keywords internal
#' @noRd
subset_scale_specs_by_output <- function(scale_specs, outputs) {
  if (is.null(scale_specs)) {
    return(NULL)
  }

  validate_scale_specs(scale_specs)

  if (!is.character(outputs)) {
    stop("`outputs` must be a character vector.", call. = FALSE)
  }

  keep <- vapply(
    scale_specs,
    function(spec) spec$output %in% outputs,
    logical(1)
  )

  scale_specs[keep]
}