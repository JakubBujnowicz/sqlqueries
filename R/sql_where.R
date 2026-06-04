#' Construct a SQL WHERE statement
#'
#' Filters rows by logical conditions. Accepts one or more conditions that
#' are combined with AND. Supports complex nested logic via `%AND%` and
#' `%OR%` operators, as well as pre-composed `sql_logical` objects.
#'
#' @param ... logical conditions to include. Can be character strings
#'   or results of other `sql` objects (e.g. [sqlqueries::sql_or()]).
#' @template param_dot-defuse
#'
#' @return A character string representing the SQL WHERE statement, with S3
#'   class 'sql_where'.
#' @export
#'
#' @family building_blocks
#'
#' @examples
#' sql_where("a = 1")
#' sql_where("a = 1", "b = 2")
#' sql_where("x = 1" %AND% "y = 2")
#'
sql_where <- function(..., .defuse = TRUE)
{
    checkmate::assert_flag(.defuse)

    condition <- sql_condition(..., .defuse = .defuse)

    rslt <- .new_sql(class = "sql_where",
                     fields = list(condition = condition))
    rslt <- .sql_parse(rslt)
    return(rslt)
}


#' Internal parser for `sql_where` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_where <- function(x, fields, ...)
{
    rslt <- paste("WHERE", fields$condition, sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}


