#' Internal SQL preparation helper
#'
#' @eval .docs_dots_columns(purpose = "names of columns", aliases = TRUE)
#' @param defuse a logical value, decides whether inputs in `...` should be
#'   defused. This includes replacing shortened calls to `sql_*()` functions
#'   with full function names.
#' @param sql_like a logical value, if `TRUE` it is required that elements
#'   in `...` are single strings or `sql` objects of classes listed in `keep`.
#'   Otherwise, all character vectors are allowed in `...`.
#' @param keep a character vector of class names, indicates allowed classes of
#'   objects to keep in the output if `sql_like = TRUE`.
#'
#' @return A list of prepared expressions/objects.
#' @keywords internal
#'
.sql_prepare <- function(..., defuse = TRUE, sql_like = TRUE,
                         keep = "sql")
{
    if (defuse) {
        message("Defusing")

        qs <- rlang::enquos(...)
        exprs <- lapply(qs, rlang::quo_get_expr)
        exprs_raw <- lapply(exprs, deparse)

        if (length(exprs) == 0) {
            stop("nothing passed to ...", call. = FALSE)
        }

        # Defusing & replacing shortened SQL calls
        sql_calls <- sapply(exprs, rlang::is_call)
        if (sum(sql_calls) > 0) {
            qs[sql_calls] <- lapply(qs[sql_calls], .defuse_calls)
        }

        evaled_exprs <- lapply(qs, rlang::eval_tidy)

    } else {
        evaled_exprs <- list(...)
        exprs_raw <- rlang::enexprs(...)
    }

    if (sql_like) {
        sql_objs <- sapply(evaled_exprs, inherits, what = keep)
        are_strings <- sapply(evaled_exprs, checkmate::test_string,
                              min.chars = 1)
        chosen <- sql_objs | are_strings
    } else {
        chosen <- sapply(evaled_exprs, checkmate::test_character,
                         any.missing = FALSE,
                         min.chars = 1,
                         min.len = 1)
    }

    not_sql <- sapply(exprs_raw[!chosen], deparse)
    if (length(not_sql) > 0) {
        warning("the following expressions were omitted:\n",
                toString(not_sql),
                call. = FALSE)
        evaled_exprs <- evaled_exprs[chosen]
    }

    if (length(evaled_exprs) == 0) {
        stop("no SQL functions nor objects passed",
             call. = FALSE)
    }

    # Extend directly supplied SQL queries
    queries <- which(sapply(evaled_exprs, inherits, what = "sql_query"))
    if (length(queries) > 0) {
        evaled_exprs <- .replace_list(x = evaled_exprs,
                                      what = lapply(evaled_exprs[queries],
                                                    attr,
                                                    which = "fields",
                                                    exact = TRUE),
                                      where = queries)
    }

    return(evaled_exprs)
}


