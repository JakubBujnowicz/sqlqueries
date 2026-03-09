#' Title
#'
#' @param x
#' @param y
#' @param operator
#'
#' @name sql_logical
#'
#' @return
#' @export
#'
#' @examples
.new_logical <- function(x, y, operator)
{
    assert_string(x, min.chars = 1L)
    assert_string(y, min.chars = 1L)

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



.parse.sql_logical <- function(x, fields, break_lines = TRUE, ...)
{
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
