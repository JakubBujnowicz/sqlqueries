.new_logical <- function(x, y, operator)
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

    rslt <- .new_sql(class = "sql_logical",
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
.sql_parse.sql_logical <- function(x, break_lines = TRUE, ...)
{
    attrs <- attributes(x)
    tree <- attrs$tree
    n <- length(tree$operators)

    sep <- ifelse(break_lines, "\n", " ")
    sep <- paste0(sep, toupper(tree$operators), " ")

    rslt <- tree$elements[[1]]
    for (i in seq_len(n)) {
        curr <- tree$elements[[i + 1]]
        if (inherits(curr, "sql_parenth")) {

            # Indent by the width of the operator
            curr <- .indent(curr, by = nchar(sep[i]) - 1)
        }

        rslt <- paste0(rslt, sep[i], curr)
    }

    attributes(rslt) <- attrs
    return(rslt)
}
