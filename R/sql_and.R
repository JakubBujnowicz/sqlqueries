#' @rdname sql_logical
#' @export
#'
sql_and <- function(x, y)
{
    rslt <- .new_logical(x = x, y = y,
                         operator = "and")
    return(rslt)
}

#' @rdname sql_logical
#' @export
#'
`%AND%` <- function(x, y)
{
    rslt <- .new_logical(x = x, y = y,
                         operator = "and")
    return(rslt)
}
