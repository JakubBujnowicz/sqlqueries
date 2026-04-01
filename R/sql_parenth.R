#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
sql_parenth <- function(x)
{
    rslt <- .new_sql(class = "sql_parenth",
                     fields = list(contains = x))
    rslt <- .sql_parse(rslt)
    return(rslt)
}



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


