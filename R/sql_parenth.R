#' Wrap `sql` objects in parentheses
#'
#' DESCRIPTION TO BE WRITTEN
#'
#' @param x a single string, in particular any `sql` object.
#'
#' @return A character string wrapped in parentheses, with S3
#'   class 'sql_parenth'.
#' @export
#'
sql_parenth <- function(x)
{
    checkmate::assert_string(x)

    rslt <- .new_sql(class = "sql_parenth",
                     fields = list(contains = x))
    rslt <- .sql_parse(rslt)
    return(rslt)
}


#' Internal parser for `sql_parenth` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_parenth <- function(x, fields, ...)
{
    contains <- fields$contains

    if (inherits(contains, "sql_logical")) {
        operators <- attr(contains, "fields", exact = TRUE)$operators
        n <- length(operators)
        contains <- .sql_parse(contains, break_lines = n >= 2)
    }

    rslt <- paste0("(", contains, ")", collapse = "")
    rslt <- .indent(rslt, by = 1)
    return(rslt)
}


