# Outer function for parsing ---------------------------------------------------
#' Parsing `sql` objects into text
#'
#' The function uses `sql_*` classes methods for `.parse` generic to transform
#' all the necessary attributes of an object (i.e. data parsed when constructing)
#' to create a formatted (parsed) text of the statement/query.
#'
#' @param x a `sql` object.
#' @param fields a list passed to `.parse` class methods. Internally set
#'   within `.sql_parse()` wrapper to `attributes(x)`. The attributes
#'   are set within object constructors.
#' @param ... another arguments passed to `.parse()` and corresponding methods.
#'
#' @return A string with a parsed object --- same attributes, but with formatted
#'   text.
#'
#' @name sql_parse
#'
#' @keywords internal
#'
.sql_parse <- function(x, ...)
{
    checkmate::assert_string(x)

    if (!is_sql(x)) {
        return(x)
    }

    message("Parsing: ", .mclass(x), "...")

    attrs <- attributes(x)
    rslt <- .parse(x = x, fields = attrs$fields, ...)
    attributes(rslt) <- attrs
    return(rslt)
}


# Internal parser method -------------------------------------------------------
#' A generic for parsing
#'
#' @rdname sql_parse
#' @keywords internal
#'
.parse <- function(x, fields, ...)
{
    UseMethod(".parse")
}




