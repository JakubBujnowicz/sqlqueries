.new_condition <- function(x, y, operator)
{
    xtree <- attr(x, "tree", exact = TRUE)
    ytree <- attr(y, "tree", exact = TRUE)

    xval <- xtree$elements
    if (is.null(xval)) {
        xval <- list(x)
    }

    yval <- ytree$elements
    if (is.null(yval)) {
        yval <- list(y)
    }

    rslt <- .new_sql(class = "sql_condition",
                     tree = list(elements = c(xval, yval),
                                 operators = c(xtree$operators,
                                               operator,
                                               ytree$operators))
    )
    rslt <- .sql_parse(rslt)
    return(rslt)
}

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
.sql_parse.sql_condition <- function(x, break_lines = TRUE, ...)
{
    attrs <- attributes(x)
    tree <- attrs$tree
    n <- length(tree$operators)

    sep <- ifelse(break_lines, "\n", " ")
    sep <- paste0(sep, toupper(tree$operators), " ")

    rslt <- tree$elements[[1]]
    for (i in seq_len(n)) {
        rslt <- paste0(rslt, sep[i], tree$elements[[i + 1]])
    }

    attributes(rslt) <- attrs
    return(rslt)
}
