#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sql_query <- function(...)
{
    ev_exprs <- .sql_defuse(...)

    # Set names
    nms <- sapply(ev_exprs, .mclass)
    names(ev_exprs) <- toupper(str_remove(nms, "^sql_"))

    rslt <- .new_sql(class = "sql_query",
                     tree = ev_exprs) |>
        .sql_parse()
    return(rslt)
}

#' @rdname sql_query
#'
sql <- sql_query


#' Title
#'
#' @param x
#' @param level
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
.sql_parse.sql_query <- function(x, level = 0, add_parenth = FALSE, ...)
{
    attrs <- attributes(x)

    rslt <- lapply(attrs$tree, .sql_parse,
                   level = level + 1,
                   # add_space = level >= 1,
                   add_parenth = level >= 1)
    rslt <- do.call(paste0, args = list(rslt, collapse = "\n"))

    if (add_parenth) {
        rslt <- paste0("(", .indent(rslt, by = 1), ")")
    }

    attributes(rslt) <- attrs
    return(rslt)
}
