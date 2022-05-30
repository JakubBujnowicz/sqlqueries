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
    rslt <- sapply(ensyms(...), deparse)
    return(rslt)
}
