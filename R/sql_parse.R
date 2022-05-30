#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @keywords internal
#'
.sql_parse <- function(x, ...)
{
    UseMethod(".sql_parse")
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#'
#' @keywords internal
#'
.sql_parse.default <- function(x, ...)
{
    x
}
