is_sql <- function(x)
{
    inherits(x, "sql")
}

sql_tree <- function(x)
{
    assert_class(x, classes = "sql")

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

sql_vars <- function(...)
{
    exprs <- enexprs(...)
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
