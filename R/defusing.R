#' SQL defusing
#'
#' In `sqlqueries` package, defusing is used to simplify writing queries using
#' the power of Non-Standard Evaluation (NSE). This allows for:
#' * using short-form keywords instead of fully qualified function names,
#'   e.g. `sql(select("*"))` instead of `sql(sql_select("*"))`,
#' * wrapping elements of queries in parentheses preserves them in the query,
#'   e.g. `sql_condition(("a" %OR% "b") %AND% "c")`,
#' * converting unevaluated expressions to strings like in [sqlqueries::sql_vars()].
#'
#' In case one wants to use e.g. `select()` from another package or does not want
#' to rely on NSE, `.defuse` argument may be set to `FALSE` to disable this
#' behavior.
#' Defusing is turned on by default in all functions, as this significantly shortens
#' writing longer queries with several keywords.
#'
#' @name sql_defusing
#'
NULL


#' Defuse internal calls
#'
#' @param quo quosure to defuse. Should be a call, see [rlang::is_call()].
#'
#' @return The quosure with a modified expression.
#' @keywords internal
#'
.defuse_calls <- function(quo)
{
    checkmate::assert_class(quo, classes = "quosure")

    .cancel_defusing <- function(call)
    {
        nm <- rlang::call_name(call)
        call <- rlang::call_match(call, fn = get(nm), defaults = TRUE)
        call$`.defuse` <- FALSE
        return(call)
    }

    .defuse <- function(call)
    {
        # Handle shortened calls, e.g. select() to sql_select()
        nm <- rlang::call_name(call)
        if (!is.null(nm) && nm %in% .sql$keywords) {
            call <- .rename_call(call, name = paste0("sql_", nm))

        } else if (!is.null(nm) && nm == "(") {
            # Handle inserted calls to `(` and replace with sql_parenth()
            call <- .rename_call(call, "sql_parenth")
        }

        # First defusing takes care of every call in the call fields,
        # so no need to do it again
        defusable <- rlang::is_call(call, name = .sql$defusables)
        if (defusable) {
            call <- .cancel_defusing(call)
        }

        # Apply to further calls within 'call'
        calls <- sapply(call, rlang::is_call,
                        name = c(.sql$allfuns, .sql$keywords, "("))
        if (sum(calls) > 0) {
            call[calls] <- lapply(call[calls], .defuse)
        }

        return(call)
    }

    call <- rlang::quo_get_expr(quo)
    quo <- rlang::quo_set_expr(quo, .defuse(call))

    return(quo)
}


