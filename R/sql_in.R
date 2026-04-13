#' Construct a SQL IN statement
#'
#' DESCRIPTION TO BE WRITTEN
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


