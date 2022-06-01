#' @rdname sql_logical
#' @export
#'
sql_or <- function(x, y, add_parenth = TRUE)
{
    rslt <- .new_logical(x = x, y = y,
                         keyword = "or",
                         add_parenth = add_parenth)
    return(rslt)
}


#' @rdname sql_logical
#' @export
#'
`%OR%` <- function(x, y)
{
    rslt <- .new_logical(x = x, y = y,
                         keyword = "or",
                         add_parenth = FALSE)
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
.sql_parse.sql_or <- function(x, level = 0,
                              ...)
{
    rslt <- .parse_logical(x, keyword = "OR", level = level, ...)
    return(rslt)
}
