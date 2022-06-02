#' @rdname sql_logical
#' @export
#'
sql_or <- function(x, y)
{
    rslt <- .new_condition(x = x, y = y,
                           operator = "or")
    return(rslt)
}


#' @rdname sql_logical
#' @export
#'
`%OR%` <- function(x, y)
{
    rslt <- .new_condition(x = x, y = y,
                           operator = "or")
    return(rslt)
}
