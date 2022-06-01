#' Title
#'
#' @param x
#' @param y
#'
#' @return
#'
#' @name sql_logical
#'
#' @examples
NULL


#' Title
#'
#' @param x
#' @param y
#' @param keyword
#'
#' @return
#'
#' @keywords internal
#'
.new_logical <- function(x, y, keyword, add_parenth, ...)
{
    assert_string(x, min.chars = 1L)
    assert_string(y, min.chars = 1L)
    assert_flag(add_parenth)

    rslt <- .new_sql(class = paste0("sql_", keyword),
                     tree = list(x = x,
                                 y = y,
                                 ...))
    attr(rslt, "add_parenth") <- add_parenth
    rslt <- .sql_parse(rslt)
    return(rslt)
}


#' Title
#'
#' @param x
#' @param keyword
#' @param ...
#'
#' @return
#'
#' @keywords internal
#'
.parse_logical <- function(x, keyword,
                           level = 0L,
                           ...)
{
    attrs <- attributes(x)
    tree <- attrs$tree

    # Set level to 0 if X and Y are simple, not SQL strings and parenthesis are
    # on
    if (attrs$add_parenth && !is_sql(tree$x) && !is_sql(tree$y)) {
        level <- 0L
    }

    sep <- ifelse(level >= 1L, "\n", " ")
    sep <- paste0(sep, keyword, " ")

    rslt <- paste(.sql_parse(tree$x, level = level + 1L),
                  .sql_parse(tree$y, level = level + 1L),
                  sep = sep)

    if (attrs$add_parenth) {
        rslt <- paste0("(", .indent(rslt, by = 1L), ")")
    }

    attributes(rslt) <- attrs
    return(rslt)
}
