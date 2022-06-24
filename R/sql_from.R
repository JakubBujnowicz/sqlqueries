#' Title
#'
#' @param table
#' @param alias
#'
#' @return
#' @export
#'
#' @examples
sql_from <- function(table, alias = NULL)
{
    assert_string(table, min.chars = 1)
    assert_string(alias, null.ok = TRUE,
                  min.chars = 1)

    rslt <- .new_sql(class = "sql_from",
                     fields = list(table = table,
                                 alias = alias))
	rslt <- .sql_parse(rslt)
    return(rslt)
}
