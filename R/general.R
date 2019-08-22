#' Generate a required or optional arguments to a LaTeX call.
#'
#' @param x A character scalar (or something that can be coerced into a
#'   character scalar)
#' @param ... Named arguments that must be present for the optional argument to
#'   work.
#' @return The character scalar in French braces, "{x}".
#' @examples
#' latex_required_arg("A")
#' latex_optional_arg(NULL)
#' @export
latex_required_arg <- function(x) {
  UseMethod("latex_required_arg")
}

latex_required_arg.default <- function(x) {
  if (is.null(x)) {
    stop("`x` may not be NULL.")
  } else if (length(x) != 1) {
    stop("`x` must be a scalar.")
  }
  sprintf("{%s}", as.character(x))
}

latex_required_arg.list <- function(x) {
  if (length(x)) {
    if (any(missing_required <- sapply(X=x, FUN=is.null))) {
      stop(
        "One or more required argument is NULL: ",
        paste(names(x)[missing_required], collapse=", ")
      )
    }
    paste(sapply(X=x, FUN=latex_required_arg), collapse="")
  } else {
    ""
  }
}

#' @describeIn latex_required_arg Generate optional arguments to a LaTeX call.
#' @export
latex_optional_arg <- function(x=NULL, ...) {
  UseMethod("latex_optional_arg")
}

latex_optional_arg.default <- function(x=NULL, ...) {
  if (is.null(x)) {
    ""
  } else {
    if (length(x) != 1) {
      stop("`x` must be a scalar.")
    }
    args <- list(...)
    if (length(args)) {
      if (is.null(names(args)) || any(names(args) == "")) {
        stop("All optional arguments must be named.")
      }
      for (arg_idx in seq_along(args)) {
        if (is.null(args[[arg_idx]])) {
          stop("Previous optional argument ", names(args)[[arg_idx]], " must not be NULL.")
        }
      }
    }
    sprintf("[%s]", as.character(x))
  }
}

latex_optional_arg.list <- function(x=NULL, ...) {
  if (length(x)) {
    null_optional <- sapply(X=x, FUN=is.null)
    if (any(null_optional) &&
        !all(null_optional) &&
        (min(which(null_optional)) < max(which(!null_optional)))) {
      stop("An optional argument is NULL before an optional argument that is not NULL.")
    }
    paste(sapply(X=x, FUN=latex_optional_arg), collapse="")
  } else {
    ""
  }
}

#' Generate a LaTeX command
#'
#' @inheritParams latex_environment
#' @param brace_protect If there are no \code{required_args}, should empty
#'   braces, \code{\\{\\}}, be added to the end of the command?
#' @return A character scalar of the command
#' @examples
#' latex_command("newpar")
#' latex_command("newpar", brace_protect=FALSE)
#' latex_command("section", required_args="My first section")
#' latex_command("frac", required_args=list(1, 2))
#' @export
latex_command <- function(x, required_args=list(), optional_args=list(), brace_protect=TRUE) {
  opts <- latex_optional_arg(optional_args)
  reqs <- latex_required_arg(required_args)
  if (nchar(reqs) == 0 & brace_protect) {
    reqs <- "{}"
  }
  sprintf("\\%s%s%s", x, opts, reqs)
}

#' Generate a LaTeX environment
#'
#' @param x The contents of the environment, a scalar vector where it will be
#'   collapsed with newlines.
#' @param environment_name The name of the environment (like "tabular" or
#'   "minipage")
#' @param required_args A list of the required arguments to the environment, in
#'   order.
#' @param optional_args A named list of the optional arguments to the
#'   environment, in order.  It is an error for an optional argument to be NULL
#'   prior to a non-optional argument.
#' @return A character scalar of the environment with newlines as "\\n"
#'   characters embedded.
#' @export
latex_environment <- function(x, environment_name, required_args=list(), optional_args=list()) {
  optional_args_char <- latex_optional_arg(optional_args)
  required_args_char <- latex_required_arg(required_args)
  contents <- paste(c(as.character(x), ""), collapse="\n")
  sprintf(
    "\\begin{%s}%s%s\n%s\\end{%s}",
    environment_name, optional_args_char, required_args_char,
    contents,
    environment_name
  )
}
