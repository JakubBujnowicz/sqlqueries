is_sql <- function(x)
{
    inherits(x, "sql")
}

sql_tree <- function(x)
{
    assert_class(x, classes = "sql")

    .get_tree <- function(obj)
    {
        tree <- attr(obj, "tree", exact = TRUE)
        are_sqls <- sapply(tree, is_sql)
        tree[are_sqls] <- lapply(tree[are_sqls], .get_tree)
        return(tree)
    }

    tree <- .get_tree(x)
    return(tree)
}

sql_vars <- function(...)
{
    exprs <- enexprs(...)
    symbs <- sapply(exprs, rlang::is_symbol)
    minus_calls <- sapply(exprs, rlang::is_call, name = "-")

    pick <- symbs | minus_calls
    depexpr <- sapply(exprs, deparse)
    if (!all(pick)) {
        warning("the following expressions omitted:\n",
                depexpr[!pick])
    }

    if (sum(minus_calls) > 0L) {
        exprs[minus_calls] <- lapply(exprs[minus_calls],
                                     function(x) x[[2]])
        depexpr[minus_calls] <- paste0(sapply(exprs[minus_calls], deparse),
                                       " DESC")
    }

    rslt <- depexpr[pick]
    return(rslt)
}
