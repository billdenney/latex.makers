#' Generate a LaTeX minipage environment
#'
#' @details If \code{x} is a character vector, all text will be inserted as
#'   separate paragraphs (with an empty line between).  If \code{x} is a list of
#'   character vectors, then multiple minipages will be generated (via sapply).
#'   The latter is useful for building tables of minipages, for instance.
#'
#' @param x The text to go within the minipage.  See Details.
#' @param position The position of the minipage with respect to the line where
#'   it appears.
#' @param width The width of the minipage ("\\columnwidth" is a useful choice.)
#' @param height (optional) the height of the minipage
#' @param inner_position The position of the contents within the minipage (only
#'   available if \code{height} is also provided).
#' @return Either a character scalar (if \code{x} is character input) or a
#'   character vector (if \code{x} is a list of character vectors).
#' @export
latex_minipage <- function(x, width=NULL, position=NULL, height=NULL, inner_position=NULL) {
  if (!is.null(position)) {
    position <- match.arg(position, choices=c("c", "m", "t", "b"))
  }
  if (!is.null(inner_position)) {
    inner_position <- match.arg(inner_position, choices=c("t", "c", "b", "s"))
  }
  if (is.list(x)) {
    sapply(X=x, FUN=latex_minipage, width=width, position=position, height=height, inner_position=inner_position)
  } else {
    latex_environment(
      x=paste(x, collapse="\n"),
      environment_name="minipage",
      required_args=list(width=width),
      optional_args=list(position=position, height=height, inner_position=inner_position)
    )
  }
}
