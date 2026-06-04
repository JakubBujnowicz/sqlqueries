#' Create SQL queries
#'
#' DESCRIPTION TO BE WRITTEN
#'
#' `sql()` is a shorthand for `sql_query()`.
#'
#' @param ... elements of the query, either single strings or other `sql` objects.
#' @param .glue `NULL` or a list, data.frame or environment to glue strings
#'   with [sqlqueries::sql_glue()]. No gluing is done if `NULL` is provided.
#' @template param_dot-defuse
#'
#' @return A character string with a SQL query, with S3 class 'sql_query'.
#' @export
#'
sql_query <- function(..., .glue = NULL, .defuse = TRUE)
{
    # Assertions
    checkmate::assert_flag(.defuse)

    ev_exprs <- .sql_prepare(..., defuse = .defuse)

    # Glue variables, must be done here and separately for every element,
    # because if only the parsed string of sql_query() is modified, the glueing
    # is lost after another element is added to the query
    if (!is.null(.glue)) {
        ev_exprs <- lapply(ev_exprs, sql_glue, .x = .glue)
    }

    # Set names
    nms <- sapply(ev_exprs, .main_class)
    names(ev_exprs) <- toupper(str_remove(nms, "^sql_"))

    rslt <- .new_sql(class = "sql_query",
                     fields = ev_exprs)
    rslt <- .sql_parse(rslt)


    return(rslt)
}


#' @rdname sql_query
#'
sql <- sql_query


#' Internal parser for `sql_query` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_query <- function(x, fields, ...)
{
    rslt <- do.call(paste0, args = list(fields, collapse = "\n"))
    return(rslt)
}


