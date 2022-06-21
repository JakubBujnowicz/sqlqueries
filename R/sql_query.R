#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sql_query <- function(..., .glue = NULL, .defuse = TRUE)
{
    assert_flag(.defuse)

    ev_exprs <- .sql_prepare(..., defuse = .defuse)

    # Set names
    nms <- sapply(ev_exprs, .mclass)
    names(ev_exprs) <- toupper(str_remove(nms, "^sql_"))

    rslt <- .new_sql(class = "sql_query",
                     tree = ev_exprs) |>
        .sql_parse()

    if (!is.null(.glue)) {
        rslt <- sql_glue(rslt, .x = .glue)
    }

    return(rslt)
}


#' @rdname sql_query
#'
sql <- sql_query


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
.sql_parse.sql_query <- function(x, glue, ...)
{
    attrs <- attributes(x)

    rslt <- do.call(paste0, args = list(attrs$tree, collapse = "\n"))

    attributes(rslt) <- attrs
    return(rslt)
}
