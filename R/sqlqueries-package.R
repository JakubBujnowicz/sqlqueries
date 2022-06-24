#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import checkmate
#' @import rlang
#' @importFrom glue glue_data
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
## usethis namespace: end
NULL

# Internal package variables ---------------------------------------------------
.sql <- within(list(),
               {
                   keywords <- c("query", "select", "from", "where",
                                 "and", "or", "vars", "join", "having",
                                 "group_by", "order_by", "tuple")
                   keyfuns <- paste0("sql_", keywords)
                   infix_funs <- paste0("%", c("OR", "AND"), "%")
                   allfuns <- c(keyfuns, infix_funs,
                                ".sql_parenth")

                   defusables <- paste0("sql_", c("query", "select", "where"))
               })
