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
    assert_flag(.defuse)

    condition <- sql_condition(..., .defuse = .defuse)

    rslt <- .new_sql(class = "sql_where",
                     fields = list(condition = condition))
    rslt <- .sql_parse(rslt)
    return(rslt)
}
