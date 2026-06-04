#' Construct a SQL GROUP BY statement
#'
#' Groups rows by the specified columns for use with aggregate functions.
#' Multiple columns can be provided and are included in the GROUP BY clause
#' in the order given.
#'
#' @eval .docs_dots_columns(purpose = "columns to group by")
#' @template param_dot-defuse
#'
#' @return A character string representing the SQL GROUP BY statement, with S3
#'   class 'sql_group_by'.
#' @export
#'
#' @family building_blocks
#'
#' @examples
#' sql_group_by("a")
#' sql_group_by("a", "b")
#'
sql_group_by <- function(..., .defuse = TRUE)
{
    assert_flag(.defuse)

    cols <- .columns_picker(..., .defuse = .defuse)

    rslt <- .new_sql(class = "sql_group_by",
                     fields = list(columns = cols))
    rslt <- .sql_parse(rslt)
    return(rslt)
}


#' Internal parser for `sql_group_by` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_group_by <- function(x, fields, ...)
{
    header <- "GROUP BY"
    rslt <- paste(header, .columns_formatter(fields$columns), sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}


