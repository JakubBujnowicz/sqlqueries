#' Print SQL objects
#'
#' Prints a SQL object to the console with a decorative header showing
#' the package name and a dashed line separator above and below the SQL
#' text. This method is automatically invoked when a `sql` object is
#' printed at the command line.
#'
#' @inheritParams base::print
#'
#' @return Returns `x` invisibly.
#' @export
#'
#' @examples
#' print(sql_select("*"))
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
#' @examples
#' sql_select("*") +
#'     sql_from("dt")
#'
#' sql_select("*") +
#'     sql_from("dt") +
#'     sql_where("a > 1")
#'
`+.sql` <- function(e1, e2)
{
    sql_query(e1, e2, .defuse = FALSE)
}


