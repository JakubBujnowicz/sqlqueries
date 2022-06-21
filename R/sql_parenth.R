#' Title
#'
#' @param ...
#'
#' @return
#'
#' @keywords internal
#'
.sql_parenth <- function(x)
{
    rslt <- .new_sql(class = "sql_parenth",
                     tree = list(contains = x))
    rslt <- .sql_parse(rslt)
    return(rslt)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @keywords internal
#'
.sql_parse.sql_parenth <- function(x, ...)
{
    attrs <- attributes(x)
    contains <- attrs$tree$contains

    if (inherits(contains, "sql_condition")) {
        operators <- attr(contains, "tree", exact = TRUE)$operators
        n <- length(operators)
        contains <- .sql_parse(contains, break_lines = n >= 2)
    }

    rslt <- paste0("(", contains, ")", collapse = "")
    rslt <- .indent(rslt, by = 1)

    attributes(rslt) <- attrs
    return(rslt)
}
