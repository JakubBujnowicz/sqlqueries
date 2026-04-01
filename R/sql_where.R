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
    checkmate::assert_flag(.defuse)

    condition <- sql_condition(..., .defuse = .defuse)

    rslt <- .new_sql(class = "sql_where",
                     fields = list(condition = condition))
    rslt <- .sql_parse(rslt)
    return(rslt)
}



.parse.sql_where <- function(x, fields, ...)
{
    rslt <- paste("WHERE", fields$condition, sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}


