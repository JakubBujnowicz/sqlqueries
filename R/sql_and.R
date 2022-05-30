#' @rdname sql_logical
#' @export
#'
sql_and <- function(x, y)
{
    rslt <- .new_logical(x = x, y = y,
                         keyword = "and")
    return(rslt)
}

#' @rdname sql_logical
#' @export
#'
`%AND%` <- sql_and

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
.sql_parse.sql_and <- function(x, level = 0,
                               ...)
{
    rslt <- .parse_logical(x, keyword = "AND", level = level, ...)
    return(rslt)
}
