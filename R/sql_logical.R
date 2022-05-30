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
.new_logical <- function(x, y, keyword, ...)
{
    assert_string(x, min.chars = 1)
    assert_string(y, min.chars = 1)

    rslt <- .new_sql(class = paste0("sql_", keyword),
                     tree = list(x = x,
                                 y = y,
                                 ...)) |>
        .sql_parse()
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
                           level = 0,
                           add_parenth = FALSE,
                           add_parenth_x = FALSE,
                           add_parenth_y = FALSE,
                           ...)
{
    attrs <- attributes(x)
    tree <- attrs$tree

    if (is.function(add_parenth_x)) {
        add_parenth_x <- add_parenth_x(tree$x)
    }
    if (is.function(add_parenth_y)) {
        add_parenth_y <- add_parenth_y(tree$y)
    }

    sep <- ifelse(level >= 1, "\n", " ")
    sep <- paste0(sep, keyword, " ")

    rslt <- paste(.sql_parse(tree$x, level = level + 1,
                             add_parenth = add_parenth_x),
                  .sql_parse(tree$y, level = level + 1,
                             add_parenth = add_parenth_y),
                  sep = sep)

    if (add_parenth) {
        rslt <- paste0("(", .indent(rslt, by = 1), ")")
    }

    attributes(rslt) <- attrs
    return(rslt)
}
