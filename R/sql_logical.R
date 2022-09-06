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
