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
                     fields = list(contains = x))
    rslt <- .sql_parse(rslt)
    return(rslt)
}
