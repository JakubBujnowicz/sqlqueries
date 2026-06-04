#' Glue data into SQL objects
#'
#' A wrapper for [glue::glue_data()], which preserves attributes for `sql`
#' objects.
#'
#' Note that underlying [glue::glue_data()] is called with `.trim = FALSE`.
#'
#' @param sql a single string or a `sql` object.
#' @inheritParams glue::glue_data
#'
#' @return A single string with `"{some_variable}"` text elements replaced.
#'   If `sql` was an object of `sql` class, the class and associated attributes
#'   are preserved.
#' @export
#'
#' @family sql_utilities
#'
#' @examples
#' sql_glue("SELECT * FROM {table}",
#'         .x = list(table = "my_table"))
#'
sql_glue <- function(sql, .x, ...)
{
    checkmate::assert(
        checkmate::check_class(sql, classes = "sql"),
        checkmate::check_string(sql, min.chars = 1),
        combine = "or")

    attrs <- attributes(sql)

    rslt <- glue::glue_data(.x = .x,
                            .trim = FALSE,
                            sql,
                            ...)

    attributes(rslt) <- attrs
    return(rslt)
}


