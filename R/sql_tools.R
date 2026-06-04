#' Convert `sql` objects to list trees
#'
#' This function can be used to explore internal structure of `sql` objects,
#' including data stored in object `fields` attribute. When used e.g. on `sql_query`
#' objects, the entire tree is returned as a nested list.
#'
#' @param x a `sql` object.
#'
#' @return A list representing the tree.
#' @export
#'
#' @examples
#' sql_tree(sql_select("*"))
#' sql_tree(
#'     sql_select("*") +
#'         sql_from("dt"))
#'
sql_tree <- function(x)
{
    checkmate::assert_class(x, classes = "sql")

    .get_tree <- function(obj)
    {
        fields <- attr(obj, "fields", exact = TRUE)
        are_sqls <- sapply(fields, is_sql)
        fields[are_sqls] <- lapply(fields[are_sqls], .get_tree)
        return(fields)
    }

    fields <- .get_tree(x)
    return(fields)
}


#' Prepare a variables list using Non-Standard Evaluation
#'
#' This function converts the provided expressions to character vectors, allowing
#' for easier inputs for e.g. SELECT statements. When an expression starts with
#' a minus operator, "DESC" is appended afterwards, which can be used for
#' easier typing in ORDER BY.
#'
#' @param ... Expressions to convert into strings.
#'
#' @return A character vector of variables.
#' @export
#'
#' @family sql_utilities
#'
#' @examples
#' sql_vars(Var1 = FirstVariable, Var2)
#' sql_vars(-FirstVariable, Var2)
#'
sql_vars <- function(...)
{
    exprs <- rlang::enexprs(...)
    minus_calls <- sapply(exprs, rlang::is_call, name = "-")

    rslt <- sapply(exprs, deparse)

    if (sum(minus_calls) > 0L) {
        exprs[minus_calls] <- lapply(exprs[minus_calls],
                                     function(x) x[[2]])
        rslt[minus_calls] <- paste0(sapply(exprs[minus_calls], deparse),
                                    " DESC")
    }

    return(rslt)
}


#' Check whether object is a `sql` object
#'
#' `is_sql()` checks whether an object inherits from the general `sql` class.
#'
#' @param x Any object to be checked for `sql` class.
#'
#' @return Either `TRUE` or `FALSE`.
#' @export
#'
#' @examples
#' is_sql(sql_select("*"))
#' is_sql("raw string")
#'
is_sql <- function(x)
{
    inherits(x, "sql")
}


