#' @rdname sql_logical
#' @export
#'
sql_or <- function(x, y)
{
    rslt <- .new_logical(x = x, y = y,
                         operator = "or")
    return(rslt)
}


#' @rdname sql_logical
#' @export
#'
`%OR%` <- sql_or
