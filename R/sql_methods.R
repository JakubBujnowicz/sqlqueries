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
    k <- max(nchar(strsplit(x, "\n")[[1]]))
    cat(paste0("/* [sqlqueries] */",
               "\n", strrep("-", k),
               "\n", x,
               "\n", strrep("-", k),
               "\n"))
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
    sql_query(e1, e2, .defuse = FALSE)
}
