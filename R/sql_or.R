#' @rdname sql_logical
#' @export
#'
sql_or <- function(x, y)
{
    rslt <- .new_logical(x = x, y = y,
                         keyword = "or")
    return(rslt)
}


#' @rdname sql_logical
#' @export
#'
`%OR%` <- sql_or


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
.sql_parse.sql_or <- function(x, level = 0, add_parenth = FALSE,
                              ...)
{
    parenth_tester <- function(x) inherits(x, what = "sql_and")
    rslt <- .parse_logical(x, keyword = "OR", level = level,
                           add_parenth_x = parenth_tester,
                           add_parenth_y = parenth_tester,
                           ...)
    return(rslt)
}
