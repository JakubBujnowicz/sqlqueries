#' Print SQL objects
#'
#' DESCRIPTION TO BE WRITTEN.
#'
#' @inheritParams base::print
#'
#' @return Returns `x` invisibly.
#' @export
#'
print.sql <- function(x, ...)
{
    k <- max(nchar(strsplit(x, "\n")[[1]]))
    # Upper limit
    k <- min(50, k)
    cat(paste0("/* [sqlqueries] */",
               "\n", strrep("-", k),
               "\n", x,
               "\n", strrep("-", k),
               "\n"))
    return(invisible(x))
}


#' "Add" SQL objects
#'
#' Addition operator serves as a wrapper for composing two elements into a
#' single [sqlqueries::sql_query()].
#'
#' @param e1,e2 `sql` objects to "add" (compose into a single query).
#'
#' @return A `sql_query` object.
#' @export
#'
`+.sql` <- function(e1, e2)
{
    sql_query(e1, e2, .defuse = FALSE)
}


