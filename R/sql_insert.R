#' Create a SQL INSERT statement
#'
#' @param into a single non-empty string, the name of a table to insert into.
#' @param values a data frame with at least one row, used as VALUES for
#'   inserting. Values (columns) from the data frame are coerced into character
#'   vectors (through [sqlqueries::sql_tuple()]).
#' @param query a `sql_query` object, may be used to insert through a select
#'   query.
#' @param columns a character vector, can be used to insert into only a selected
#'   subset of columns of `into` table.
#'
#' @return A character string representing the SQL INSERT statement, with S3
#'   class 'sql_insert'.
#' @export
#'
sql_insert <- function(into, values = NULL, query = NULL, columns = NULL)
{
    checkmate::assert_string(into, min.chars = 1L)
    checkmate::assert_character(columns, min.chars = 1, min.len = 1,
                                any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_data_frame(values, min.rows = 1, null.ok = TRUE)
    if (!is.null(values)) {
        checkmate::assert_names(names(values), type = "unique",
                                must.include = columns)
    }
    checkmate::assert_class(query, classes = "sql_query", null.ok = TRUE)

    if (!xor(is.null(values), is.null(query))) {
        stop("'values' and 'query' cannot be filled in a single INSERT statement")
    }

    rslt <- .new_sql(class = "sql_insert",
                     fields = list(into = into,
                                   values = values,
                                   query = query,
                                   columns = columns))
    rslt <- .sql_parse(rslt)
    return(rslt)
}



#' Internal parser for `sql_insert` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_insert <- function(x, fields, ...)
{
    vals <- as.list(fields$values)
    cols <- fields$columns

    if (!is.null(cols)) {
        vals <- vals[cols]
        cols <- paste0("(", toString(cols), ")")
    } else {
        cols <- ""
    }

    target <- paste0("INSERT INTO\n", fields$into, " ", cols)

    if (!is.null(fields$query)) {
        what <- fields$query
    } else {
        vals <- do.call(sql_tuple, args = vals)
        vals <- .sql_parse(vals, as_values = TRUE)
        what <- paste0("VALUES\n", .indent(vals, by = 4, indent_first = TRUE))
    }

    rslt <- paste0(.indent(target, by = 4), "\n", what)
    return(rslt)
}
