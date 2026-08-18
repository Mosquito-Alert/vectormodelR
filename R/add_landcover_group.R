#' Add a supported land-cover grouping to prepared model data
#'
#' Retains land-cover categories with enough presences and absences, and groups
#' sparse categories into "Other". If "Other" remains sparse, uses Built-up
#' versus all other land-cover classes.
#'
#' @param dataset Output from `prepare_model_data()`.
#' @param reference Reference land-cover category.
#' @param min_presences Minimum presences required to retain a category.
#' @param min_absences Minimum absences required to retain a category.
#'
#' @return The prepared dataset with `landcover_group` added to `model_data`.
#' @export
add_landcover_group <- function(
  dataset,
  reference = "Built-up",
  min_presences = 10L,
  min_absences = 10L
) {
  model_data <- dataset$model_data

  required_cols <- c(
    "landcover_class",
    "presence"
  )

  missing_cols <- setdiff(
    required_cols,
    names(model_data)
  )

  if (length(missing_cols)) {
    stop(
      "`model_data` is missing: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  landcover <- as.character(
    model_data$landcover_class
  )

  presence <- as.integer(
    model_data$presence
  )

  if (!all(presence %in% c(0L, 1L, NA_integer_))) {
    stop(
      "`presence` must contain only 0, 1, or NA.",
      call. = FALSE
    )
  }

  landcover_table <- table(
    factor(landcover),
    factor(presence, levels = c(0L, 1L))
  )

  if (!reference %in% rownames(landcover_table)) {
    stop(
      "Reference category not found: ",
      reference,
      call. = FALSE
    )
  }

  supported <- rownames(landcover_table)[
    landcover_table[, "0"] >= min_absences &
      landcover_table[, "1"] >= min_presences
  ]

  if (!reference %in% supported) {
    stop(
      "The reference category does not have enough presences and absences.",
      call. = FALSE
    )
  }

  grouped <- landcover
  grouped[!grouped %in% supported] <- "Other"

  grouped_table <- table(
    factor(grouped),
    factor(presence, levels = c(0L, 1L))
  )

  other_is_sparse <- "Other" %in% rownames(grouped_table) &&
    (
      grouped_table["Other", "0"] < min_absences ||
        grouped_table["Other", "1"] < min_presences
    )

  if (other_is_sparse) {
    grouped <- ifelse(
      landcover == reference,
      reference,
      "Other"
    )
  }

  grouped_levels <- c(
    reference,
    sort(setdiff(unique(grouped), reference))
  )

  model_data$landcover_group <- factor(
    grouped,
    levels = grouped_levels
  )

  dataset$model_data <- model_data

  if (is.null(dataset$meta)) {
    dataset$meta <- list()
  }

  dataset$meta$landcover_grouping <- list(
    reference = reference,
    min_presences = min_presences,
    min_absences = min_absences,
    original_counts = landcover_table,
    grouped_counts = table(
      model_data$landcover_group,
      presence
    )
  )

  dataset
}