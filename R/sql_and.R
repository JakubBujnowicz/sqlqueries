#' @rdname sql_logical
#' @export
#'
sql_and <- function(x, y, add_parenth = TRUE)
{
    rslt <- .new_logical(x = x, y = y,
                         keyword = "and",
                         add_parent = add_parenth)
    return(rslt)
}

#' @rdname sql_logical
#' @export
#'
`%AND%` <- function(x, y)
{
    rslt <- .new_logical(x = x, y = y,
                         keyword = "and",
                         add_parent = FALSE)
    return(rslt)
}

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
