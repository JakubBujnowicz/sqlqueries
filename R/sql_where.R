#' Title
#'
#' @param condition
#'
#' @return
#' @export
#'
#' @examples
sql_where <- function(..., .defuse = TRUE)
{
    assert_flag(defuse)

    condition <- .sql_prepare(..., defuse = .defuse)
    condition <- Reduce(sql_and, condition)

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
.sql_parse.sql_where <- function(x, ...)
{
    attrs <- attributes(x)

    rslt <- paste("WHERE", attrs$tree$condition, sep = "\n") |>
        .indent(by = 4)

    attributes(rslt) <- attrs
    return(rslt)
}
