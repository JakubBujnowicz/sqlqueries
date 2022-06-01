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
                           level,
                           ...)
{
    attrs <- attributes(x)
    tree <- attrs$tree

    sep <- ifelse(level >= 1L, "\n", " ")
    sep <- paste0(sep, keyword, " ")

    tree <- lapply(tree, .sql_parse, level = level + 1L)
    rslt <- do.call(paste, args = c(tree, sep = sep))

    attributes(rslt) <- attrs
    return(rslt)
}
