.sql_defuse <- function(..., sql_like = TRUE, prefix_sql = TRUE)
{
    qs <- rlang::enquos(...)
    exprs <- lapply(qs, rlang::quo_get_expr)
    exprs_raw <- lapply(exprs, deparse)

    # Replacing shortened SQL calls
    sql_calls <- sapply(exprs, rlang::is_call)
    if (prefix_sql && sum(sql_calls) > 0) {
        qs[sql_calls] <- lapply(qs[sql_calls], .prefix_sql_callnames)
    }

    ev_exprs <- lapply(qs, rlang::eval_tidy)

    if (sql_like) {
        sql_objs <- sapply(ev_exprs, inherits, what = "sql")
        chosen <- sql_objs | sapply(ev_exprs, test_string,
                                    min.chars = 1)
    } else {
        chosen <- sapply(ev_exprs, test_character,
                         any.missing = FALSE,
                         min.chars = 1,
                         min.len = 1)
    }

    not_sql <- sapply(exprs_raw[!chosen], deparse)
    if (length(not_sql) > 0) {
        warning("the following expressions were omitted:\n",
                toString(not_sql))
        ev_exprs <- ev_exprs[chosen]
    }

    if (length(ev_exprs) == 0) {
        stop("no SQL functions nor objects passed")
    }

    # Extend inserted SQL queries of the first level
    queries <- which(sapply(ev_exprs, inherits, what = "sql_query"))
    if (length(queries) > 0) {
        ev_exprs <- .replace_list(x = ev_exprs,
                                  what = lapply(ev_exprs[queries], attr,
                                                which = "tree",
                                                exact = TRUE),
                                  where = queries)
    }

    return(ev_exprs)
}
