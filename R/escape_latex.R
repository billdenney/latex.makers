#' Escape special characters in text intended to go into a LaTeX document
#'
#' @details
#' Do not escape text before sending it to this function.
#'
#' @param x The character string to escape
#' @param ... Passed to methods
#' @return `x` with LaTeX special characters escaped
#' @export
escape_latex <- function(x, ...) {
  UseMethod("escape_latex")
}

#' @rdname escape_latex
#' @export
escape_latex.default <- function(x, ...) {
  vapply(X = x, FUN = escape_latex_helper, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

#' Escape LaTeX special characters in a single character string
#' @param x A single character string
#' @return `x` with LaTeX special characters escaped
#' @keywords internal
#' @noRd
escape_latex_helper <- function(x) {
  stopifnot(length(x) == 1)
  split_x <- strsplit(x = x, split = "", fixed = TRUE)[[1]]
  paste0(
    dplyr::case_match(
      split_x,
      "\\" ~ "\\textbackslash{}",
      "^" ~ "\\textasciicircum{}",
      "~" ~ "\\textasciitilde{}",
      "{" ~ "\\{",
      "}" ~ "\\}",
      "#" ~ "\\#",
      "$" ~ "\\$",
      "%" ~ "\\%",
      "&" ~ "\\&",
      "_" ~ "\\_",
      .default = split_x
    ),
    collapse = ""
  )
}
