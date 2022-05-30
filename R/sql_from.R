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
                     tree = list(table = table,
                                 alias = alias)) |>
        .sql_parse()
    return(rslt)
}

#' Title
#'
#' @param x
#' @param ...
#' @param level
#'
#' @return
#' @export
#'
#' @keywords internal
#'
.sql_parse.sql_from <- function(x, level = 0,
                                add_parenth = TRUE,
                                ...)
{
    assert_count(level)

    attrs <- attributes(x)
    tree <- attrs$tree

    tab_name <- names(tree$table)

    rslt <- .sql_parse(tree$table,
                       level = level + 1,
                       add_parenth = TRUE)
    if (!is.null(tree$alias)) {
        rslt <- paste0(rslt, " AS ", tree$alias)
    } else if (test_string(tab_name, min.chars = 1)) {
        rslt <- paste0(rslt, " AS ", tab_name)
    }

    rslt <- paste("FROM", rslt, sep = "\n")
    rslt <- .indent(rslt, by = 4)

    attributes(rslt) <- attrs
    return(rslt)
}
