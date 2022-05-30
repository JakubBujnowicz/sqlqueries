#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.sql <- function(x, ...)
{
    cat(x, "\n")
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
`+.sql` <- function(e1, e2)
{
    sql_query(e1, e2)
}
