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



.parse.sql_from <- function(x, fields, ...)
{
    rslt <- fields$table
    if (inherits(rslt, "sql_query")) {
        rslt <- .add_parenth(rslt)
    }

    tab_name <- names(fields$table)
    if (!is.null(fields$alias)) {
        rslt <- paste0(rslt, " AS ", fields$alias)
    } else if (test_string(tab_name, min.chars = 1)) {
        rslt <- paste0(rslt, " AS ", tab_name)
    }

    rslt <- paste("FROM", rslt, sep = "\n")
    rslt <- .indent(rslt, by = 4)

    return(rslt)
}