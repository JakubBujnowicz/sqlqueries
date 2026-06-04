#' Construct a SQL FROM statement
#'
#' Declares the source table for a query. Accepts a plain table name, a
#' named vector to define an alias, or a subquery (any `sql_query` object)
#' which is automatically wrapped in parentheses for use as a derived table.
#'
#' @param table a single string with the name of the table. In particular, a
#'  `sql_query` object may be passed as well, it is then automatically
#'  wrapped in parentheses.
#' @param alias a single string or `NULL`. If provided, serves as an alias for
#'  the table. Alias can also be provided by passing a named vector to
#'  `table` (the name cannot be empty), however `alias` takes precedence.
#'
#' @return A character string representing the SQL FROM statement, with S3
#'   class 'sql_from'.
#' @export
#'
#' @family building_blocks
#'
#' @examples
#' sql_from("dt")
#' sql_from("dt", alias = "t")
#' sql_from(c(t = "dt"))
#'
sql_from <- function(table, alias = NULL)
{
    checkmate::assert_string(table, min.chars = 1)
    checkmate::assert_string(alias, null.ok = TRUE,
                             min.chars = 1)

    rslt <- .new_sql(class = "sql_from",
                     fields = list(table = table,
                                 alias = alias))
    rslt <- .sql_parse(rslt)
    return(rslt)
}


#' Internal parser for `sql_from` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_from <- function(x, fields, ...)
{
    rslt <- fields$table
    if (inherits(rslt, "sql_query")) {
        rslt <- .add_parenth(rslt)
    }

    tab_name <- names(fields$table)
    if (!is.null(fields$alias)) {
        rslt <- paste0(rslt, " AS ", fields$alias)
    } else if (checkmate::test_string(tab_name, min.chars = 1)) {
        rslt <- paste0(rslt, " AS ", tab_name)
    }

    rslt <- paste("FROM", rslt, sep = "\n")
    rslt <- .indent(rslt, by = 4)

    return(rslt)
}


