#' Title
#'
#' @param condition
#'
#' @return
#' @export
#'
#' @examples
sql_having <- function(..., .defuse = TRUE)
{
    assert_flag(.defuse)

    condition <- sql_condition(..., .defuse = .defuse)

    rslt <- .new_sql(class = "sql_having",
                     tree = list(condition = condition))
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
.sql_parse.sql_having <- function(x, ...)
{
    attrs <- attributes(x)

    rslt <- paste("HAVING", attrs$tree$condition, sep = "\n")
    rslt <- .indent(rslt, by = 4)

    attributes(rslt) <- attrs
    return(rslt)
}
