sql_group_by <- function(..., .defuse = TRUE)
{
    assert_flag(.defuse)

    cols <- .columns_picker(..., .defuse = .defuse)

    rslt <- .new_sql(class = "sql_group_by",
                     tree = list(columns = cols))
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
.sql_parse.sql_group_by <- function(x, ...)
{
    attrs <- attributes(x)
    tree <- attrs$tree

    header <- "GROUP BY"
    rslt <- paste(header, .columns_parser(tree$columns), sep = "\n")
    rslt <- .indent(rslt, by = 4)

    attributes(rslt) <- attrs
    return(rslt)
}
