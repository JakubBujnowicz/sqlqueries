.sql_prepare <- function(..., defuse = TRUE, sql_like = TRUE,
                         keep = "sql")
{
    if (defuse) {
        message("Defusing")
        qs <- rlang::enquos(...)
        exprs <- lapply(qs, rlang::quo_get_expr)
        exprs_raw <- lapply(exprs, deparse)

        # Defusing & replacing shortened SQL calls
        sql_calls <- sapply(exprs, rlang::is_call)
        if (sum(sql_calls) > 0) {
            qs[sql_calls] <- lapply(qs[sql_calls], .defuse_calls)
        }

        ev_exprs <- lapply(qs, rlang::eval_tidy)

    } else {
        ev_exprs <- list(...)
        exprs_raw <- rlang::enexprs(...)
    }

    if (sql_like) {
        sql_objs <- sapply(ev_exprs, inherits, what = keep)
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
                toString(not_sql),
                call. = FALSE)
        ev_exprs <- ev_exprs[chosen]
    }

    if (length(ev_exprs) == 0) {
        stop("no SQL functions nor objects passed")
    }

    # Extend directly supplied SQL queries
    queries <- which(sapply(ev_exprs, inherits, what = "sql_query"))
    if (length(queries) > 0) {
        ev_exprs <- .replace_list(x = ev_exprs,
                                  what = lapply(ev_exprs[queries], attr,
                                                which = "fields",
                                                exact = TRUE),
                                  where = queries)
    }

    return(ev_exprs)
}
