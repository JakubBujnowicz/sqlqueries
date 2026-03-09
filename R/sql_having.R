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
                     fields = list(condition = condition))
    rslt <- .sql_parse(rslt)
    return(rslt)
}



.parse.sql_having <- function(x, fields, ...)
{
    rslt <- paste("HAVING", fields$condition, sep = "\n")
    rslt <- .indent(rslt, by = 4)
    return(rslt)
}