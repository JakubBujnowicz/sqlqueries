#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import checkmate
#' @import rlang
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
## usethis namespace: end
NULL

# Internal package variables ---------------------------------------------------
.sql <- within(list(),
               {
                   keywords <- c("query", "select", "from", "where",
                                 "and", "or", "vars")
                   keyfuns <- paste0("sql_", keywords)
                   infix_funs <- paste0("%", c("OR", "AND"), "%")
                   allfuns <- c(keyfuns, infix_funs,
                                ".sql_parenth")
               })
