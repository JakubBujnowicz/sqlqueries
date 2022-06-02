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
.sql_parse <- function(x, ...)
{
    message("Parsing: ", .mclass(x), "...")
        if (!is_sql(x) && test_string(x)) {
        return(x)
    }

    UseMethod(".sql_parse")
}
