#' Internal function for creating logical conditions
#'
#' This works as a template for creating logical conditions in a object-oriented
#' matter, e.g. AND or OR.
#'
#' @param x,y non-empty strings, input arguments for the logical operator.
#' @param operator a non-empty string, name of the logical operator to be
#'   used (e.g. `"and"` or `"or"`).
#'
#' @return A character string representing the logical conditions, with S3
#'   class 'sql_logical'.
#'
#' @export
#'
sql_logical <- function(x, y, operator)
{
    checkmate::assert_string(x, min.chars = 1L)
    checkmate::assert_string(y, min.chars = 1L)
    checkmate::assert_string(operator, min.chars = 1L)

    xtree <- attr(x, "fields", exact = TRUE)
    ytree <- attr(y, "fields", exact = TRUE)

    xval <- xtree$elements
    if (is.null(xval)) {
        xval <- list(x)
    }

    yval <- ytree$elements
    if (is.null(yval)) {
        yval <- list(y)
    }

    rslt <- .new_sql(class = "sql_logical",
                     fields = list(elements = c(xval, yval),
                                   operators = c(xtree$operators,
                                                 operator,
                                                 ytree$operators))
    )
    rslt <- .sql_parse(rslt)
    return(rslt)
}


#' @rdname sql_logical
#'
#' @param ... Arguments to be processed, representing conditions.
#' @template param_dot-defuse
#'
#' @export
#'
sql_condition <- function(..., .defuse = TRUE)
{
    checkmate::assert_flag(.defuse)

    condition <- .sql_prepare(..., defuse = .defuse)
    condition <- Reduce(sql_and, condition)

    return(condition)
}


# Operators --------------------------------------------------------------------
#' @rdname sql_logical
#' @export
#'
sql_and <- function(x, y)
{
    rslt <- sql_logical(x = x, y = y,
                        operator = "and")
    return(rslt)
}


#' @rdname sql_logical
#' @export
#'
sql_or <- function(x, y)
{
    rslt <- sql_logical(x = x, y = y,
                        operator = "or")
    return(rslt)
}


#' @rdname sql_logical
#' @export
#'
`%AND%` <- sql_and


#' @rdname sql_logical
#' @export
#'
`%OR%` <- sql_or


# Parsing ----------------------------------------------------------------------
#' Internal parser for `sql_logical` objects
#'
#' @inheritParams sql_parse
#' @keywords internal
#'
.parse.sql_logical <- function(x, fields, break_lines = TRUE, ...)
{
    checkmate::assert_flag(break_lines)

    n <- length(fields$operators)

    sep <- ifelse(break_lines, "\n", " ")
    sep <- paste0(sep, toupper(fields$operators), " ")

    rslt <- fields$elements[[1]]
    for (i in seq_len(n)) {
        curr <- fields$elements[[i + 1]]

        if (inherits(curr, "sql_parenth")) {
            # Indent by the width of the operator
            curr <- .indent(curr, by = nchar(sep[i]) - 1)
        }

        rslt <- paste0(rslt, sep[i], curr)
    }

    return(rslt)
}


