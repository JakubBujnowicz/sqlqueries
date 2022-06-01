#' Title
#'
#' @param ...
#'
#' @return
#'
#' @keywords internal
#'
.sql_parenth <- function(...)
{
    rslt <- .new_sql(class = "sql_parenth",
                     tree = list(elements = list(...)))
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
    tree <- attrs$tree
    n <- length(tree)
    level <- as.integer(n > 2)
    # print(setNames(lapply(tree$elements, unclass), n))

    rslt <- sapply(tree$elements, .sql_parse, level = level)
    rslt <- paste0("(", rslt, ")") |>
        .indent(by = 1)

    attributes(rslt) <- attrs
    return(rslt)
}
