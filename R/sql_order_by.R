#' Construct a SQL ORDER BY statement
#'
#' Sorts the result set by the specified columns. Each column can be
#' sorted in ascending (default) or descending order by prefixing with
#' a minus sign via [sql_vars()].
#'
#' @eval .docs_dots_columns(purpose = "columns to order by")
#' @template param_dot-defuse
#'
#' @return A character string representing the SQL ORDER BY statement, with S3
#'   class 'sql_order_by'.
#' @export
#'
#' @family building_blocks
#'
#' @examples
#' sql_order_by("a", "b")
#' sql_order_by(vars(a, -b))
#'
sql_order_by <- function(..., .defuse = TRUE)
{
    checkmate::assert_flag(.defuse)

    cols <- .columns_picker(..., .defuse = .defuse)

    rslt <- .new_sql(class = "sql_order_by",
                     fields = list(columns = cols))
    rslt <- .sql_parse(rslt)
    return(rslt)
}



#' Internal parser for `sql_order_by` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_order_by <- function(x, fields, ...)
{
    header <- "ORDER BY"
    rslt <- paste(header, .columns_formatter(fields$columns), sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}
