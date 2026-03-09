# Outer function for parsing ---------------------------------------------------
.sql_parse <- function(x, ...)
{
    assert_string(x)

    if (!is_sql(x)) {
        return(x)
    }

    message("Parsing: ", .mclass(x), "...")

    attrs <- attributes(x)
    rslt <- .parse(x = x, fields = attrs$fields, ...)
    attributes(rslt) <- attrs
    return(rslt)
}


# Inner parser with methods for each class -------------------------------------
#' Title
#'
#' @param x
#' @param fields
#' @param ...
#'
#' @return
#' @export
#'
#' @name sql_parse
#'
#' @keywords internal
#'
.parse <- function(x, fields, ...)
{
    UseMethod(".parse")
}
