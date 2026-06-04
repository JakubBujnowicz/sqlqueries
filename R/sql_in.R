#' Construct a SQL IN statement
#'
#' Creates an IN condition that checks whether a column value matches any
#' element in a given set. The set can be a vector of values, which is
#' automatically formatted as a SQL tuple. The `%IN%` infix operator
#' provides a shorthand alternative.
#'
#' `x %IN% y` is a wrapper for `sql_in(x, vector = y)`.
#'
#' @param x a single string, usually a name of a variable
#' @param vector a single string or an atomic vector. Represents the set of
#'   allowed values for the IN statement. If a vector of at least two elements
#'   is provided, then `vector` is passed to [sqlqueries::sql_tuple()].
#'
#' @return A character string representing the SQL IN statement, with S3
#'   class 'sql_in'.
#' @export
#'
#' @examples
#' sql_in("col1", 1:3)
#' sql_in("col1", c("a", "b", "c"))
#' "col1" %IN% 1:3
#'
sql_in <- function(x, vector)
{
    checkmate::assert_string(x, min.chars = 1L)
    checkmate::assert(
        checkmate::check_string(vector, min.chars = 1L),
        checkmate::check_atomic(vector, any.missing = FALSE,
                                min.len = 1))

    rslt <- .new_sql(class = "sql_in",
                     fields = list(x = x,
                                   vector = vector))
    rslt <- .sql_parse(rslt)
    return(rslt)
}


#' @rdname sql_in
#' @export
#'
`%IN%` <- sql_in


#' Internal parser for `sql_in` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_in <- function(x, fields, ...)
{
    vec <- unique(fields$vector)
    if (length(vec) > 1) {
        vec <- sql_tuple(vec)
    }

    rslt <- paste(fields$x, "IN", vec)
    return(rslt)
}


