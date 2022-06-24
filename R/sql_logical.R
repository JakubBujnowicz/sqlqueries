.new_logical <- function(x, y, operator)
{
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
