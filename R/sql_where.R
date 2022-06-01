#' Title
#'
#' @param condition
#'
#' @return
#' @export
#'
#' @examples
sql_where <- function(...)
{
    condition <- .sql_defuse(...)
    condition <- Reduce(`%AND%`, condition)

    rslt <- .new_sql(class = "sql_where",
                     tree = list(condition = condition)) |>
        .sql_parse()
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
.sql_parse.sql_where <- function(x, level = 0,
                                  ...)
{
    attrs <- attributes(x)

    rslt <- .sql_parse(attrs$tree$condition, level = level + 1)
    rslt <- paste("WHERE", rslt, sep = "\n") |>
        .indent(by = 4)

    attributes(rslt) <- attrs
    return(rslt)
}
