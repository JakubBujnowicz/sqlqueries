#' Construct a SQL CASE WHEN statement
#'
#' DESCRIPTION TO BE WRITTEN
#'
#' @param ... an atomic vector of conditions and values. Must be of even length.
#'   Represents a sequence of `when1`, `then1`, `when2`, `then2` and so on...
#' @param .else optional scalar value. If not `NULL`, then the provided value
#'   is used for the finale ELSE statement.
#'
#' @return A character string representing the SQL CASE WHEN statement, with S3
#'   class 'sql_case'.
#' @export
#'
sql_case <- function(..., .else = NULL)
{
    x <- c(...)
    checkmate::assert_atomic_vector(x, min.len = 2,
                                    .var.name = "...")
    checkmate::assert_true(length(x) %% 2 == 0,
                           .var.name = "length of '...' is even")
    checkmate::assert_scalar(.else, null.ok = TRUE)

    rslt <- .new_sql(class = "sql_case",
                     fields = list(cases = x,
                                   .else = .else))
    rslt <- .sql_parse(rslt)
    return(rslt)
}

#' Internal parser for `sql_case` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_case <- function(x, fields, ...)
{
    x <- fields$cases
    n <- length(x)
    thens <- seq_len(n / 2L) * 2L
    whens <- thens - 1L

    rslt <- paste0("\nWHEN ", format(x[whens]), " THEN ", x[thens])
    rslt <- paste0(rslt, collapse = "")
    if (!is.null(fields$.else)) {
        rslt <- paste0(rslt, "\nELSE ", fields$.else)
    }

    rslt <- paste0("CASE", .indent(rslt, by = 4L), "\nEND")
    return(rslt)
}


