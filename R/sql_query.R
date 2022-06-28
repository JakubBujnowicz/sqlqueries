#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sql_query <- function(..., glue = NULL, .defuse = TRUE)
{
    # Assertions
    assert_flag(.defuse)

    ev_exprs <- .sql_prepare(..., defuse = .defuse)

    # Set names
    nms <- sapply(ev_exprs, .mclass)
    names(ev_exprs) <- toupper(str_remove(nms, "^sql_"))

    rslt <- .new_sql(class = "sql_query",
                     fields = ev_exprs)
    rslt <- .sql_parse(rslt)

    if (!is.null(.glue)) {
        rslt <- sql_glue(rslt, .x = glue)
    }

    return(rslt)
}


#' @rdname sql_query
#'
sql <- sql_query
